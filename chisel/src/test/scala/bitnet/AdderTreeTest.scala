package bitnet

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class AdderTreeTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val cfg: BitNetConfig = BitNetConfig(numPEs = 64, avalonDataW = 128)

  behavior of "AdderTree"

  it should "sum all-zeros to zero (T-MAC 32 inputs)" in {
    test(new AdderTree(cfg.numGroups, cfg.groupOutW, cfg.accumW)) { dut =>
      for (i <- 0 until cfg.numGroups) {
        dut.io.inputs(i).poke(0.S)
      }
      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.valid_in.poke(false.B)

      dut.clock.step(cfg.tmacTreePipeStages - 1)
      dut.io.valid_out.expect(true.B)
      dut.io.sum.expect(0.S)
    }
  }

  it should "sum all-ones to numGroups" in {
    test(new AdderTree(cfg.numGroups, cfg.groupOutW, cfg.accumW)) { dut =>
      for (i <- 0 until cfg.numGroups) {
        dut.io.inputs(i).poke(1.S)
      }
      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.valid_in.poke(false.B)

      dut.clock.step(cfg.tmacTreePipeStages - 1)
      dut.io.valid_out.expect(true.B)
      dut.io.sum.expect(cfg.numGroups.S)
    }
  }

  it should "sum alternating +1/-1 to zero" in {
    test(new AdderTree(cfg.numGroups, cfg.groupOutW, cfg.accumW)) { dut =>
      for (i <- 0 until cfg.numGroups) {
        if (i % 2 == 0) dut.io.inputs(i).poke(1.S)
        else dut.io.inputs(i).poke(-1.S)
      }
      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.valid_in.poke(false.B)

      dut.clock.step(cfg.tmacTreePipeStages - 1)
      dut.io.valid_out.expect(true.B)
      dut.io.sum.expect(0.S)
    }
  }

  it should "correctly sum a known pattern" in {
    test(new AdderTree(cfg.numGroups, cfg.groupOutW, cfg.accumW)) { dut =>
      val values = Seq(10, 20, 30, 40) ++ Seq.fill(cfg.numGroups - 4)(0)
      for (i <- 0 until cfg.numGroups) {
        dut.io.inputs(i).poke(values(i).S)
      }
      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.valid_in.poke(false.B)

      dut.clock.step(cfg.tmacTreePipeStages - 1)
      dut.io.valid_out.expect(true.B)
      dut.io.sum.expect(100.S)
    }
  }

  it should "have pipeline latency of tmacTreePipeStages cycles" in {
    test(new AdderTree(cfg.numGroups, cfg.groupOutW, cfg.accumW)) { dut =>
      for (i <- 0 until cfg.numGroups) {
        dut.io.inputs(i).poke(0.S)
      }
      dut.io.valid_in.poke(false.B)

      for (_ <- 0 until cfg.tmacTreePipeStages) {
        dut.io.valid_out.expect(false.B)
        dut.clock.step(1)
      }

      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.valid_in.poke(false.B)

      for (i <- 0 until cfg.tmacTreePipeStages - 1) {
        dut.io.valid_out.expect(false.B, s"valid_out should be false at step $i")
        dut.clock.step(1)
      }
      dut.io.valid_out.expect(true.B, "valid_out should be true after pipeline latency")
    }
  }
}
