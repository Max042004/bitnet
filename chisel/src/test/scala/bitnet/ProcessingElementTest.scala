package bitnet

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ProcessingElementTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val cfg: BitNetConfig = BitNetConfig(numPEs = 64, avalonDataW = 128)

  behavior of "ProcessingElement"

  it should "output 0 when enable is false" in {
    test(new ProcessingElement) { dut =>
      for (act <- Seq(0, 1, -1, 127, -128)) {
        dut.io.activation.poke(act.S)
        dut.io.enable.poke(false.B)
        dut.io.sign.poke(false.B)
        dut.io.result.expect(0.S)
        dut.io.sign.poke(true.B)
        dut.io.result.expect(0.S)
      }
    }
  }

  it should "pass-through activation when enable=1, sign=0 (weight=+1)" in {
    test(new ProcessingElement) { dut =>
      dut.io.enable.poke(true.B)
      dut.io.sign.poke(false.B)
      for (act <- Seq(0, 1, -1, 127, -128, 42, -42)) {
        dut.io.activation.poke(act.S)
        dut.io.result.expect(act.S)
      }
    }
  }

  it should "negate activation when enable=1, sign=1 (weight=-1)" in {
    test(new ProcessingElement) { dut =>
      dut.io.enable.poke(true.B)
      dut.io.sign.poke(true.B)
      for (act <- Seq(0, 1, -1, 127, -128, 42, -42)) {
        dut.io.activation.poke(act.S)
        dut.io.result.expect((-act).S)
      }
    }
  }

  it should "handle all 3 weight states x edge activations exhaustively" in {
    test(new ProcessingElement) { dut =>
      val activations = Seq(0, 1, -1, 127, -128)
      val weightStates = Seq(
        (false, false, 0),  // enable=0 → 0
        (true, false, 1),   // enable=1, sign=0 → +1
        (true, true, -1)    // enable=1, sign=1 → -1
      )

      var count = 0
      for ((enable, sign, weight) <- weightStates) {
        for (act <- activations) {
          dut.io.activation.poke(act.S)
          dut.io.enable.poke(enable.B)
          dut.io.sign.poke(sign.B)
          val expected = act * weight
          dut.io.result.expect(expected.S,
            s"act=$act, enable=$enable, sign=$sign: expected $expected")
          count += 1
        }
      }
      println(s"Verified $count PE combinations")
    }
  }
}
