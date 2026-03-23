package bitnet

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TmacLutBuilderTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val cfg: BitNetConfig = BitNetConfig(numPEs = 8, avalonDataW = 16, maxDimK = 8, maxDimM = 4)

  behavior of "TmacLutBuilder"

  /** Reference: compute expected LUT for a group of 4 activations */
  def buildRefLut(x: Seq[Int]): Seq[Int] = {
    require(x.length == 4)
    (0 until 16).map { mask =>
      var sum = 0
      for (i <- 0 until 4) if ((mask & (1 << i)) != 0) sum += x(i)
      sum
    }
  }

  it should "build correct LUT for simple activations [1, 2, 3, 4]" in {
    test(new TmacLutBuilder) { dut =>
      // numPEs=8, groupSize=4, numGroups=2
      // Group 0: activations[0..3] = [1, 2, 3, 4]
      // Group 1: activations[4..7] = [0, 0, 0, 0]
      val acts = Seq(1, 2, 3, 4, 0, 0, 0, 0)
      for (i <- 0 until cfg.numPEs) {
        dut.io.activations(i).poke(acts(i).S)
      }

      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      // Wait for done (4 cycles)
      var cycles = 0
      while (!dut.io.done.peek().litToBoolean && cycles < 10) {
        dut.clock.step(1)
        cycles += 1
      }
      assert(cycles <= 4, s"LUT build took $cycles cycles, expected <= 4")

      // Verify group 0 LUT
      val refLut0 = buildRefLut(Seq(1, 2, 3, 4))
      for (e <- 0 until 16) {
        val actual = dut.io.luts(0)(e).peek().litValue.toInt
        assert(actual == refLut0(e), s"Group 0, entry $e: expected ${refLut0(e)}, got $actual")
      }

      // Verify group 1 LUT (all zeros)
      for (e <- 0 until 16) {
        dut.io.luts(1)(e).expect(0.S)
      }
    }
  }

  it should "build correct LUT for negative activations [-10, 20, -30, 40]" in {
    test(new TmacLutBuilder) { dut =>
      val acts = Seq(-10, 20, -30, 40, 5, -5, 10, -10)
      for (i <- 0 until cfg.numPEs) {
        dut.io.activations(i).poke(acts(i).S)
      }

      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)
      dut.clock.step(4)

      val refLut0 = buildRefLut(Seq(-10, 20, -30, 40))
      for (e <- 0 until 16) {
        val actual = dut.io.luts(0)(e).peek().litValue.toInt
        // Handle signed: litValue returns unsigned, convert
        val signed = if (actual >= (1 << (cfg.lutEntryW - 1))) actual - (1 << cfg.lutEntryW) else actual
        assert(signed == refLut0(e), s"Group 0, entry $e: expected ${refLut0(e)}, got $signed")
      }

      val refLut1 = buildRefLut(Seq(5, -5, 10, -10))
      for (e <- 0 until 16) {
        val actual = dut.io.luts(1)(e).peek().litValue.toInt
        val signed = if (actual >= (1 << (cfg.lutEntryW - 1))) actual - (1 << cfg.lutEntryW) else actual
        assert(signed == refLut1(e), s"Group 1, entry $e: expected ${refLut1(e)}, got $signed")
      }
    }
  }

  it should "complete in exactly 4 cycles" in {
    test(new TmacLutBuilder) { dut =>
      for (i <- 0 until cfg.numPEs) {
        dut.io.activations(i).poke(i.S)
      }

      dut.io.done.expect(false.B)
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      // Should not be done during build
      for (c <- 0 until 3) {
        dut.io.done.expect(false.B, s"Should not be done at cycle $c")
        dut.clock.step(1)
      }
      // Done after 4th build cycle
      dut.io.done.expect(true.B, "Should be done after 4 cycles")
    }
  }
}
