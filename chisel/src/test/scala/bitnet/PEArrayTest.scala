package bitnet

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class PEArrayTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val cfg: BitNetConfig = BitNetConfig()

  behavior of "PEArray"

  it should "compute parallel ternary products matching software model" in {
    test(new PEArray) { dut =>
      // Set up a known pattern:
      // activations: 1, 2, 3, ..., 64
      // weights: alternating +1, -1, 0, +1, -1, 0, ...
      val activations = (1 to cfg.numPEs).toSeq
      val weights = (0 until cfg.numPEs).map(i => (i % 3) match {
        case 0 => 1   // +1: enable=1, sign=0
        case 1 => -1  // -1: enable=1, sign=1
        case 2 => 0   //  0: enable=0
      })

      for (i <- 0 until cfg.numPEs) {
        dut.io.activations(i).poke(activations(i).S)
        weights(i) match {
          case 1  => dut.io.enables(i).poke(true.B); dut.io.signs(i).poke(false.B)
          case -1 => dut.io.enables(i).poke(true.B); dut.io.signs(i).poke(true.B)
          case 0  => dut.io.enables(i).poke(false.B); dut.io.signs(i).poke(false.B)
        }
      }

      // Verify each PE output
      for (i <- 0 until cfg.numPEs) {
        val expected = activations(i) * weights(i)
        dut.io.results(i).expect(expected.S,
          s"PE $i: act=${activations(i)}, weight=${weights(i)}, expected=$expected")
      }
    }
  }

  it should "output all zeros when all weights are zero" in {
    test(new PEArray) { dut =>
      for (i <- 0 until cfg.numPEs) {
        dut.io.activations(i).poke((i + 1).S)
        dut.io.enables(i).poke(false.B)
        dut.io.signs(i).poke(false.B)
      }
      for (i <- 0 until cfg.numPEs) {
        dut.io.results(i).expect(0.S)
      }
    }
  }

  it should "pass through activations when all weights are +1" in {
    test(new PEArray) { dut =>
      for (i <- 0 until cfg.numPEs) {
        val act = ((i * 7 + 3) % 256) - 128  // Pseudo-random INT8 values
        dut.io.activations(i).poke(act.S)
        dut.io.enables(i).poke(true.B)
        dut.io.signs(i).poke(false.B)
      }
      for (i <- 0 until cfg.numPEs) {
        val act = ((i * 7 + 3) % 256) - 128
        dut.io.results(i).expect(act.S)
      }
    }
  }

  it should "negate all activations when all weights are -1" in {
    test(new PEArray) { dut =>
      for (i <- 0 until cfg.numPEs) {
        val act = ((i * 7 + 3) % 256) - 128
        dut.io.activations(i).poke(act.S)
        dut.io.enables(i).poke(true.B)
        dut.io.signs(i).poke(true.B)
      }
      for (i <- 0 until cfg.numPEs) {
        val act = ((i * 7 + 3) % 256) - 128
        dut.io.results(i).expect((-act).S)
      }
    }
  }
}
