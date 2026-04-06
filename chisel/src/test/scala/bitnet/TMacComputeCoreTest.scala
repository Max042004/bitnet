package bitnet

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TMacComputeCoreTest extends AnyFlatSpec with ChiselScalatestTester {
  // Small config for testing: P=4 engines, K=12 (4 groups, 1 tile)
  implicit val cfg: TMacConfig = TMacConfig(numEngines = 4, maxDimK = 48, maxDimM = 16)

  /** Reference LUT computation matching biturbo.c tmac_build_three_lut */
  def refLut(a0: Int, a1: Int, a2: Int): Array[Int] = {
    Array(
      0, a2, a1, a0,
      a1 + a2, a0 + a2, a0 + a1,
      a1 - a2, a0 - a2, a0 - a1,
      a0 + a1 + a2, a0 + a1 - a2, a0 - a1 + a2, a0 - a1 - a2,
      0, 0
    )
  }

  /** Pack 16 LUT entries into a 256-bit word */
  def packLut(entries: Array[Int]): BigInt = {
    var packed = BigInt(0)
    for (i <- 0 until 16) {
      val unsigned = if (entries(i) < 0) entries(i) + 65536 else entries(i)
      packed = packed | (BigInt(unsigned & 0xFFFF) << (i * 16))
    }
    packed
  }

  behavior of "TMacComputeCore"

  it should "compute a single-tile GEMV row with all +1 weights" in {
    test(new TMacComputeCore) { dut =>
      // 4 groups, activations: (1,2,3), (4,5,6), (7,8,9), (10,11,12)
      // All-+1 weights: nibble=10 (index for all +1 = a0+a1+a2), sign=0
      // Expected per group: 1+2+3=6, 4+5+6=15, 7+8+9=24, 10+11+12=33
      // Total: 6+15+24+33 = 78

      val activations = Array(
        Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9), Array(10, 11, 12)
      )

      // Pre-load LUT data (simulate what LutBuilder would produce)
      val lutWords = activations.map { a => packLut(refLut(a(0), a(1), a(2))) }

      // Nibble 10 = index for a0+a1+a2 entry
      // Pack nibbles: 4 engines × 4 bits = 16 bits, rest is 0
      var nibbles = BigInt(0)
      for (e <- 0 until cfg.numEngines) {
        nibbles = nibbles | (BigInt(10) << (e * 4))
      }

      // Signs: all 0 (no negation)
      val signs = BigInt(0)

      // Set up tile processing
      dut.io.numGroups.poke(4.U)
      dut.io.tileIdx.poke(0.U)

      // Present LUT data (simulate BRAM read output)
      for (e <- 0 until cfg.numEngines) {
        dut.io.lutReadData(e).poke(lutWords(e).U)
      }

      // Start row
      dut.io.rowStart.poke(true.B)
      dut.io.tileIn.poke(true.B)
      dut.io.nibbles.poke(nibbles.U)
      dut.io.signs.poke(signs.U)
      dut.io.tileValid.poke(true.B)
      dut.clock.step(1)
      dut.io.rowStart.poke(false.B)
      dut.io.tileIn.poke(false.B)
      dut.io.tileValid.poke(false.B)

      // Wait for pipeline: 1 (BRAM) + 1 (engine reg) + treeDepth + 4 (margin)
      dut.clock.step(1 + 1 + cfg.treeDepth + 4)

      val accumVal = dut.io.accumOut.peek().litValue.toInt
      println(s"Accumulator value: $accumVal (expected: 78)")
      assert(accumVal == 78, s"Expected 78, got $accumVal")
    }
  }

  it should "compute with sign correction (negated groups)" in {
    test(new TMacComputeCore) { dut =>
      // 4 groups, same activations as above
      // Nibble 10 (a0+a1+a2), but sign=1 for groups 2 and 3
      // Expected: 6 + 15 - 24 - 33 = -36

      val activations = Array(
        Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9), Array(10, 11, 12)
      )
      val lutWords = activations.map { a => packLut(refLut(a(0), a(1), a(2))) }

      var nibbles = BigInt(0)
      for (e <- 0 until cfg.numEngines) {
        nibbles = nibbles | (BigInt(10) << (e * 4))
      }

      // Signs: bits 2 and 3 set (negate groups 2 and 3)
      val signs = BigInt(0x0C) // binary 1100

      dut.io.numGroups.poke(4.U)
      dut.io.tileIdx.poke(0.U)

      for (e <- 0 until cfg.numEngines) {
        dut.io.lutReadData(e).poke(lutWords(e).U)
      }

      dut.io.rowStart.poke(true.B)
      dut.io.tileIn.poke(true.B)
      dut.io.nibbles.poke(nibbles.U)
      dut.io.signs.poke(signs.U)
      dut.io.tileValid.poke(true.B)
      dut.clock.step(1)
      dut.io.rowStart.poke(false.B)
      dut.io.tileIn.poke(false.B)
      dut.io.tileValid.poke(false.B)

      dut.clock.step(1 + 1 + cfg.treeDepth + 4)

      val rawVal = dut.io.accumOut.peek().litValue
      // Handle signed interpretation of accumW-bit value
      val accumVal = if (rawVal.testBit(cfg.accumW - 1)) {
        (rawVal - (BigInt(1) << cfg.accumW)).toInt
      } else rawVal.toInt

      println(s"Accumulator value: $accumVal (expected: -36)")
      assert(accumVal == -36, s"Expected -36, got $accumVal")
    }
  }

  it should "compute with nibble=0 (all weights zero)" in {
    test(new TMacComputeCore) { dut =>
      // Nibble 0 → LUT entry 0 → value 0 for all groups
      val activations = Array(
        Array(10, 20, 30), Array(40, 50, 60), Array(70, 80, 90), Array(100, 110, 120)
      )
      val lutWords = activations.map { a => packLut(refLut(a(0), a(1), a(2))) }

      val nibbles = BigInt(0) // All nibbles = 0
      val signs = BigInt(0)

      dut.io.numGroups.poke(4.U)
      dut.io.tileIdx.poke(0.U)

      for (e <- 0 until cfg.numEngines) {
        dut.io.lutReadData(e).poke(lutWords(e).U)
      }

      dut.io.rowStart.poke(true.B)
      dut.io.tileIn.poke(true.B)
      dut.io.nibbles.poke(nibbles.U)
      dut.io.signs.poke(signs.U)
      dut.io.tileValid.poke(true.B)
      dut.clock.step(1)
      dut.io.rowStart.poke(false.B)
      dut.io.tileIn.poke(false.B)
      dut.io.tileValid.poke(false.B)

      dut.clock.step(1 + 1 + cfg.treeDepth + 4)

      val accumVal = dut.io.accumOut.peek().litValue.toInt
      println(s"Accumulator value: $accumVal (expected: 0)")
      assert(accumVal == 0, s"Expected 0, got $accumVal")
    }
  }
}
