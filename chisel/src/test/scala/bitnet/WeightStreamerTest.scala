package bitnet

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class WeightStreamerTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val cfg: BitNetConfig = BitNetConfig(numPEs = 64, avalonDataW = 128)

  behavior of "WeightStreamer (continuous streaming)"

  it should "stream a single tile for M=1 row" in {
    test(new WeightStreamer) { dut =>
      // M=1, pre-computed tile address = 0x1000
      dut.io.tileAddr.poke(0x1000.U)
      dut.io.dimM.poke(1.U)

      dut.io.avalon.waitrequest.poke(false.B)
      dut.io.avalon.readdatavalid.poke(false.B)
      dut.io.avalon.readdata.poke(0.U)
      dut.io.dequeue.poke(false.B)

      // Start tile stream
      dut.io.startTile.poke(true.B)
      dut.clock.step(1)
      dut.io.startTile.poke(false.B)

      // Burst read issued
      dut.io.avalon.read.expect(true.B)
      dut.io.avalon.address.expect(0x1000.U)
      dut.io.avalon.burstcount.expect(1.U)
      dut.clock.step(1)

      // Provide data
      dut.io.avalon.readdatavalid.poke(true.B)
      dut.io.avalon.readdata.poke(0xAB.U)
      dut.clock.step(1)
      dut.io.avalon.readdatavalid.poke(false.B)

      // FIFO has data
      dut.io.dataReady.expect(true.B)
      dut.io.weightData.expect(0xAB.U)

      // Dequeue
      dut.io.dequeue.poke(true.B)
      dut.clock.step(1)
      dut.io.dequeue.poke(false.B)

      dut.io.dataReady.expect(false.B)
      dut.io.streamDone.expect(true.B)
    }
  }

  it should "stream M=4 tiles sequentially" in {
    test(new WeightStreamer) { dut =>
      // M=4, pre-computed tile address = 0x0
      dut.io.tileAddr.poke(0x0.U)
      dut.io.dimM.poke(4.U)

      dut.io.avalon.waitrequest.poke(false.B)
      dut.io.avalon.readdatavalid.poke(false.B)
      dut.io.dequeue.poke(false.B)

      dut.io.startTile.poke(true.B)
      dut.clock.step(1)
      dut.io.startTile.poke(false.B)

      // Should issue burst of 4 beats
      dut.io.avalon.read.expect(true.B)
      dut.io.avalon.burstcount.expect(4.U)
      dut.clock.step(1)

      // Provide 4 beats
      for (i <- 0 until 4) {
        dut.io.avalon.readdatavalid.poke(true.B)
        dut.io.avalon.readdata.poke((0x10 + i).U)
        dut.clock.step(1)
      }
      dut.io.avalon.readdatavalid.poke(false.B)

      // Dequeue all 4
      for (i <- 0 until 4) {
        dut.io.dataReady.expect(true.B)
        dut.io.weightData.expect((0x10 + i).U)
        dut.io.dequeue.poke(true.B)
        dut.clock.step(1)
      }
      dut.io.dequeue.poke(false.B)

      dut.io.streamDone.expect(true.B)
    }
  }

  it should "use pre-computed tile address directly" in {
    test(new WeightStreamer) { dut =>
      // Pre-computed: base(0) + tileIdx(1) * stride(32) = 32
      dut.io.tileAddr.poke(32.U)
      dut.io.dimM.poke(2.U)

      dut.io.avalon.waitrequest.poke(false.B)
      dut.io.avalon.readdatavalid.poke(false.B)
      dut.io.dequeue.poke(false.B)

      dut.io.startTile.poke(true.B)
      dut.clock.step(1)
      dut.io.startTile.poke(false.B)

      dut.io.avalon.read.expect(true.B)
      dut.io.avalon.address.expect(32.U)
      dut.io.avalon.burstcount.expect(2.U)
    }
  }

  it should "handle waitrequest delays" in {
    test(new WeightStreamer) { dut =>
      dut.io.tileAddr.poke(0x2000.U)
      dut.io.dimM.poke(1.U)

      dut.io.avalon.waitrequest.poke(true.B)
      dut.io.avalon.readdatavalid.poke(false.B)
      dut.io.dequeue.poke(false.B)

      dut.io.startTile.poke(true.B)
      dut.clock.step(1)
      dut.io.startTile.poke(false.B)

      // Held by waitrequest
      dut.io.avalon.read.expect(true.B)
      dut.clock.step(1)
      dut.io.avalon.read.expect(true.B)
      dut.clock.step(1)

      // Release
      dut.io.avalon.waitrequest.poke(false.B)
      dut.clock.step(1)

      // Provide data
      dut.io.avalon.readdatavalid.poke(true.B)
      dut.io.avalon.readdata.poke(0xCD.U)
      dut.clock.step(1)
      dut.io.avalon.readdatavalid.poke(false.B)

      dut.io.dataReady.expect(true.B)
      dut.io.weightData.expect(0xCD.U)
    }
  }
}
