package bitnet

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class WeightDecoderTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val cfg: BitNetConfig = BitNetConfig(numPEs = 64, avalonDataW = 128)

  behavior of "WeightDecoder"

  it should "decode 00 as disabled (weight=0)" in {
    test(new WeightDecoder) { dut =>
      dut.io.packed.poke(0.U)  // All zeros = all disabled
      for (i <- 0 until cfg.numPEs) {
        dut.io.enable(i).expect(false.B, s"PE $i should be disabled for encoding 00")
      }
    }
  }

  it should "decode 01 as weight=+1 (enable=1, sign=0)" in {
    test(new WeightDecoder) { dut =>
      // Set PE 0 to encoding 01: bit0=1, bit1=0
      dut.io.packed.poke(1.U)
      dut.io.enable(0).expect(true.B)
      dut.io.sign(0).expect(false.B)
    }
  }

  it should "decode 10 as weight=-1 (enable=1, sign=1)" in {
    test(new WeightDecoder) { dut =>
      // Set PE 0 to encoding 10: bit0=0, bit1=1
      dut.io.packed.poke(2.U)
      dut.io.enable(0).expect(true.B)
      dut.io.sign(0).expect(true.B)
    }
  }

  it should "decode 11 as disabled (reserved)" in {
    test(new WeightDecoder) { dut =>
      // Set PE 0 to encoding 11: bit0=1, bit1=1 → XOR = 0
      dut.io.packed.poke(3.U)
      dut.io.enable(0).expect(false.B, "Reserved encoding 11 should disable PE")
    }
  }

  it should "decode a known 128-bit pattern correctly" in {
    test(new WeightDecoder) { dut =>
      // Build a pattern: PE0=01(+1), PE1=10(-1), PE2=00(0), PE3=01(+1), rest=00
      // PE0 bits [1:0] = 01 → 0x01
      // PE1 bits [3:2] = 10 → 0x08
      // PE2 bits [5:4] = 00 → 0x00
      // PE3 bits [7:6] = 01 → 0x40
      // bits = 01_00_10_01
      // PE0 = bits[1:0] = 01 → enable=1, sign=0
      // PE1 = bits[3:2] = 10 → enable=1, sign=1
      // PE2 = bits[5:4] = 00 → enable=0
      // PE3 = bits[7:6] = 01 → enable=1, sign=0
      val packedVal = BigInt(1) | (BigInt(2) << 2) | (BigInt(0) << 4) | (BigInt(1) << 6)
      dut.io.packed.poke(packedVal.U)

      dut.io.enable(0).expect(true.B)
      dut.io.sign(0).expect(false.B)

      dut.io.enable(1).expect(true.B)
      dut.io.sign(1).expect(true.B)

      dut.io.enable(2).expect(false.B)

      dut.io.enable(3).expect(true.B)
      dut.io.sign(3).expect(false.B)
    }
  }

  it should "decode all 64 PEs from a fully packed word" in {
    test(new WeightDecoder) { dut =>
      // Alternating pattern: +1, -1, +1, -1, ...
      // +1 = 01, -1 = 10
      var packed = BigInt(0)
      for (i <- 0 until cfg.numPEs) {
        if (i % 2 == 0) {
          packed = packed | (BigInt(1) << (i * 2))   // 01 = +1
        } else {
          packed = packed | (BigInt(2) << (i * 2))   // 10 = -1
        }
      }
      dut.io.packed.poke(packed.U)

      for (i <- 0 until cfg.numPEs) {
        dut.io.enable(i).expect(true.B, s"PE $i should be enabled")
        if (i % 2 == 0) {
          dut.io.sign(i).expect(false.B, s"PE $i should be +1")
        } else {
          dut.io.sign(i).expect(true.B, s"PE $i should be -1")
        }
      }
    }
  }
}
