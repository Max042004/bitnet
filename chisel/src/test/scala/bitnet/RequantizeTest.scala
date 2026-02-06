package bitnet

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class RequantizeTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val cfg: BitNetConfig = BitNetConfig()

  behavior of "Requantize"

  it should "right-shift positive values correctly" in {
    test(new Requantize) { dut =>
      dut.io.in.poke(256.S)       // 256 >> 2 = 64
      dut.io.shiftAmt.poke(2.U)
      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.valid_in.poke(false.B)
      dut.io.out.expect(64.S)
      dut.io.valid_out.expect(true.B)
    }
  }

  it should "right-shift negative values with sign extension" in {
    test(new Requantize) { dut =>
      dut.io.in.poke(-256.S)      // -256 >> 2 = -64 (arithmetic shift)
      dut.io.shiftAmt.poke(2.U)
      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.valid_in.poke(false.B)
      dut.io.out.expect(-64.S)
      dut.io.valid_out.expect(true.B)
    }
  }

  it should "clamp positive overflow to 127" in {
    test(new Requantize) { dut =>
      dut.io.in.poke(1024.S)      // 1024 >> 1 = 512 → clamp to 127
      dut.io.shiftAmt.poke(1.U)
      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.out.expect(127.S)
    }
  }

  it should "clamp negative overflow to -128" in {
    test(new Requantize) { dut =>
      dut.io.in.poke(-1024.S)     // -1024 >> 1 = -512 → clamp to -128
      dut.io.shiftAmt.poke(1.U)
      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.out.expect(-128.S)
    }
  }

  it should "pass through zero unmodified" in {
    test(new Requantize) { dut =>
      dut.io.in.poke(0.S)
      dut.io.shiftAmt.poke(5.U)
      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.out.expect(0.S)
    }
  }

  it should "handle shift amount of 0" in {
    test(new Requantize) { dut =>
      dut.io.in.poke(42.S)
      dut.io.shiftAmt.poke(0.U)
      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.out.expect(42.S)
    }
  }

  it should "handle boundary value 127 without clamping" in {
    test(new Requantize) { dut =>
      dut.io.in.poke(127.S)
      dut.io.shiftAmt.poke(0.U)
      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.out.expect(127.S)
    }
  }

  it should "handle boundary value -128 without clamping" in {
    test(new Requantize) { dut =>
      dut.io.in.poke(-128.S)
      dut.io.shiftAmt.poke(0.U)
      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.out.expect(-128.S)
    }
  }

  it should "propagate valid signal with 1-cycle latency" in {
    test(new Requantize) { dut =>
      dut.io.in.poke(0.S)
      dut.io.shiftAmt.poke(0.U)
      dut.io.valid_in.poke(false.B)
      dut.clock.step(1)
      dut.io.valid_out.expect(false.B)

      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.valid_out.expect(true.B)

      dut.io.valid_in.poke(false.B)
      dut.clock.step(1)
      dut.io.valid_out.expect(false.B)
    }
  }
}
