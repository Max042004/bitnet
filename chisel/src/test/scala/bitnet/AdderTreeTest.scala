package bitnet

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class AdderTreeTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val cfg: BitNetConfig = BitNetConfig()

  behavior of "AdderTree"

  it should "sum all-zeros to zero with correct pipeline latency" in {
    test(new AdderTree) { dut =>
      // Drive all zeros
      for (i <- 0 until cfg.numPEs) {
        dut.io.inputs(i).poke(0.S)
      }
      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.valid_in.poke(false.B)

      // Wait for pipeline stages
      dut.clock.step(cfg.treePipeStages - 1)
      dut.io.valid_out.expect(true.B)
      dut.io.sum.expect(0.S)
    }
  }

  it should "sum all-ones to numPEs" in {
    test(new AdderTree) { dut =>
      for (i <- 0 until cfg.numPEs) {
        dut.io.inputs(i).poke(1.S)
      }
      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.valid_in.poke(false.B)

      dut.clock.step(cfg.treePipeStages - 1)
      dut.io.valid_out.expect(true.B)
      dut.io.sum.expect(cfg.numPEs.S)
    }
  }

  it should "sum alternating +1/-1 to zero" in {
    test(new AdderTree) { dut =>
      for (i <- 0 until cfg.numPEs) {
        if (i % 2 == 0) dut.io.inputs(i).poke(1.S)
        else dut.io.inputs(i).poke(-1.S)
      }
      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.valid_in.poke(false.B)

      dut.clock.step(cfg.treePipeStages - 1)
      dut.io.valid_out.expect(true.B)
      dut.io.sum.expect(0.S)
    }
  }

  it should "correctly sum a known pattern" in {
    test(new AdderTree) { dut =>
      // First 4 PEs get values 10, 20, 30, 40; rest are 0
      val values = Seq(10, 20, 30, 40) ++ Seq.fill(cfg.numPEs - 4)(0)
      for (i <- 0 until cfg.numPEs) {
        dut.io.inputs(i).poke(values(i).S)
      }
      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.valid_in.poke(false.B)

      dut.clock.step(cfg.treePipeStages - 1)
      dut.io.valid_out.expect(true.B)
      dut.io.sum.expect(100.S)
    }
  }

  it should "have pipeline latency of treePipeStages cycles" in {
    test(new AdderTree) { dut =>
      for (i <- 0 until cfg.numPEs) {
        dut.io.inputs(i).poke(0.S)
      }
      dut.io.valid_in.poke(false.B)

      // valid should be false initially
      for (_ <- 0 until cfg.treePipeStages) {
        dut.io.valid_out.expect(false.B)
        dut.clock.step(1)
      }

      // Now pulse valid_in
      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.valid_in.poke(false.B)

      // valid_out should appear exactly treePipeStages-1 steps later
      for (i <- 0 until cfg.treePipeStages - 1) {
        dut.io.valid_out.expect(false.B, s"valid_out should be false at step $i")
        dut.clock.step(1)
      }
      dut.io.valid_out.expect(true.B, "valid_out should be true after pipeline latency")
    }
  }

  it should "handle maximum negative values" in {
    test(new AdderTree) { dut =>
      // All PEs output -128 (min INT8 after PE)
      val minVal = -(1 << (cfg.activationW - 1))  // -128
      for (i <- 0 until cfg.numPEs) {
        dut.io.inputs(i).poke(minVal.S)
      }
      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.valid_in.poke(false.B)

      dut.clock.step(cfg.treePipeStages - 1)
      dut.io.valid_out.expect(true.B)
      val expected = minVal * cfg.numPEs
      dut.io.sum.expect(expected.S)
    }
  }
}
