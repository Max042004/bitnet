package bitnet

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ComputeCoreTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val cfg: BitNetConfig = BitNetConfig(numPEs = 64, avalonDataW = 128)

  behavior of "ComputeCore"

  it should "compute a single tile dot product" in {
    test(new ComputeCore) { dut =>
      // Set up: all activations = 1, all weights = +1 (encoding 01)
      // Expected sum = 64
      for (i <- 0 until cfg.numPEs) {
        dut.io.activations(i).poke(1.S)
      }
      dut.io.dimK.poke(cfg.numPEs.U)
      dut.io.shiftAmt.poke(0.U)

      // All-+1 weight word: each PE gets encoding 01 → bits = 01_01_01...
      var packed = BigInt(0)
      for (i <- 0 until cfg.numPEs) {
        packed = packed | (BigInt(1) << (i * 2))
      }

      // Start new row
      dut.io.rowStart.poke(true.B)
      dut.io.tileIn.poke(true.B)
      dut.io.weightData.poke(packed.U)
      dut.io.weightValid.poke(true.B)
      dut.clock.step(1)
      dut.io.rowStart.poke(false.B)
      dut.io.tileIn.poke(false.B)
      dut.io.weightValid.poke(false.B)

      // Wait for pipeline to flush
      dut.clock.step(cfg.treePipeStages + 4)

      // Check accumulator output
      val accumVal = dut.io.accumOut.peek().litValue.toInt
      println(s"Accumulator value: $accumVal (expected: ${cfg.numPEs})")
      assert(accumVal == cfg.numPEs, s"Expected ${cfg.numPEs}, got $accumVal")
    }
  }

  it should "compute dot product with mixed weights" in {
    test(new ComputeCore) { dut =>
      // Activations: all = 10
      // Weights: first half +1, second half -1
      // Expected: 32*10 - 32*10 = 0
      for (i <- 0 until cfg.numPEs) {
        dut.io.activations(i).poke(10.S)
      }
      dut.io.dimK.poke(cfg.numPEs.U)
      dut.io.shiftAmt.poke(0.U)

      var packed = BigInt(0)
      for (i <- 0 until cfg.numPEs) {
        if (i < cfg.numPEs / 2) {
          packed = packed | (BigInt(1) << (i * 2))   // 01 = +1
        } else {
          packed = packed | (BigInt(2) << (i * 2))   // 10 = -1
        }
      }

      dut.io.rowStart.poke(true.B)
      dut.io.tileIn.poke(true.B)
      dut.io.weightData.poke(packed.U)
      dut.io.weightValid.poke(true.B)
      dut.clock.step(1)
      dut.io.rowStart.poke(false.B)
      dut.io.tileIn.poke(false.B)
      dut.io.weightValid.poke(false.B)

      dut.clock.step(cfg.treePipeStages + 4)

      val accumVal = dut.io.accumOut.peek().litValue.toInt
      println(s"Accumulator value: $accumVal (expected: 0)")
      assert(accumVal == 0)
    }
  }

  it should "accumulate across multiple tiles" in {
    test(new ComputeCore) { dut =>
      // 2 tiles, each with activations=1, weights=+1
      // Expected: 64 + 64 = 128
      for (i <- 0 until cfg.numPEs) {
        dut.io.activations(i).poke(1.S)
      }
      dut.io.dimK.poke((cfg.numPEs * 2).U)
      dut.io.shiftAmt.poke(0.U)

      var packed = BigInt(0)
      for (i <- 0 until cfg.numPEs) {
        packed = packed | (BigInt(1) << (i * 2))
      }

      // Tile 0
      dut.io.rowStart.poke(true.B)
      dut.io.tileIn.poke(true.B)
      dut.io.weightData.poke(packed.U)
      dut.io.weightValid.poke(true.B)
      dut.clock.step(1)
      dut.io.rowStart.poke(false.B)
      dut.io.tileIn.poke(false.B)
      dut.io.weightValid.poke(false.B)

      // Wait for pipeline
      dut.clock.step(cfg.treePipeStages + 2)

      // Tile 1
      dut.io.tileIn.poke(true.B)
      dut.io.weightData.poke(packed.U)
      dut.io.weightValid.poke(true.B)
      dut.clock.step(1)
      dut.io.tileIn.poke(false.B)
      dut.io.weightValid.poke(false.B)

      // Wait for second tile
      dut.clock.step(cfg.treePipeStages + 4)

      val accumVal = dut.io.accumOut.peek().litValue.toInt
      println(s"Accumulator value: $accumVal (expected: ${cfg.numPEs * 2})")
      assert(accumVal == cfg.numPEs * 2)
    }
  }
}
