package bitnet

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class LutBuilderTest extends AnyFlatSpec with ChiselScalatestTester {
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

  behavior of "LutBuilder"

  it should "compute correct LUT entries for one group" in {
    test(new LutBuilder).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val a0 = 10
      val a1 = -5
      val a2 = 3
      val expected = refLut(a0, a1, a2)

      dut.io.start.poke(true.B)
      dut.io.numGroups.poke(1.U)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      val addr0 = dut.io.actAddr0.peek().litValue.toInt
      val addr1 = dut.io.actAddr1.peek().litValue.toInt
      val addr2 = dut.io.actAddr2.peek().litValue.toInt
      assert(addr0 == 0, s"Expected actAddr0=0, got $addr0")
      assert(addr1 == 1, s"Expected actAddr1=1, got $addr1")
      assert(addr2 == 2, s"Expected actAddr2=2, got $addr2")

      dut.io.actData0.poke(a0.S)
      dut.io.actData1.poke(a1.S)
      dut.io.actData2.poke(a2.S)
      dut.clock.step(1) // sRead -> sCapture
      dut.clock.step(1) // sCapture -> sCompute
      dut.clock.step(1) // sCompute -> sWrite

      assert(dut.io.lutWriteEn.peek().litToBoolean, "Expected LUT write enable")
      assert(dut.io.lutWriteBank.peek().litValue == 0, "Expected bank 0 for group 0")
      assert(dut.io.lutWriteAddr.peek().litValue == 0, "Expected addr 0 for group 0")

      val packedData = dut.io.lutWriteData.peek().litValue
      for (i <- 0 until 16) {
        val entry = ((packedData >> (i * cfg.lutWidth)) & ((BigInt(1) << cfg.lutWidth) - 1)).toInt
        val signed = if (entry >= 32768) entry - 65536 else entry
        assert(signed == expected(i), s"LUT entry[$i]: expected ${expected(i)}, got $signed")
      }

      dut.clock.step(1)
      assert(dut.io.done.peek().litToBoolean, "Expected done signal")
    }
  }

  it should "build LUTs for multiple groups across banks" in {
    test(new LutBuilder) { dut =>
      val activations = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)

      dut.io.start.poke(true.B)
      dut.io.numGroups.poke(4.U)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      for (g <- 0 until 4) {
        val a0 = activations(g * 3)
        val a1 = activations(g * 3 + 1)
        val a2 = activations(g * 3 + 2)
        val expected = refLut(a0, a1, a2)

        dut.io.actData0.poke(a0.S)
        dut.io.actData1.poke(a1.S)
        dut.io.actData2.poke(a2.S)
        dut.clock.step(1) // sRead -> sCapture
        dut.clock.step(1) // sCapture -> sCompute
        dut.clock.step(1) // sCompute -> sWrite

        assert(dut.io.lutWriteEn.peek().litToBoolean, s"Group $g: expected write enable")
        val bank = dut.io.lutWriteBank.peek().litValue.toInt
        assert(bank == g % cfg.numEngines, s"Group $g: expected bank ${g % cfg.numEngines}, got $bank")

        val packedData = dut.io.lutWriteData.peek().litValue
        for (i <- 0 until 16) {
          val entry = ((packedData >> (i * cfg.lutWidth)) & ((BigInt(1) << cfg.lutWidth) - 1)).toInt
          val signed = if (entry >= 32768) entry - 65536 else entry
          assert(signed == expected(i), s"Group $g, entry[$i]: expected ${expected(i)}, got $signed")
        }

        dut.clock.step(1) // sWrite -> sRead (or sDone)
      }

      assert(dut.io.done.peek().litToBoolean, "Expected done after all groups")
    }
  }
}
