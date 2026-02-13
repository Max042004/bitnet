package bitnet

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class WeightStreamerTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val cfg: BitNetConfig = BitNetConfig()

  behavior of "WeightStreamer"

  it should "fetch a single-beat burst (K=64, 1 tile)" in {
    test(new WeightStreamer) { dut =>
      dut.io.baseAddr.poke(0x1000.U)
      dut.io.dimK.poke(64.U)
      dut.io.rowIdx.poke(0.U)

      dut.io.avalon.waitrequest.poke(false.B)
      dut.io.avalon.readdatavalid.poke(false.B)
      dut.io.avalon.readdata.poke(0.U)
      dut.io.dequeue.poke(false.B)

      // Cycle 0: startRow pulse → sRead
      dut.io.startRow.poke(true.B)
      dut.clock.step(1)
      dut.io.startRow.poke(false.B)

      // Cycle 1: sRead. read=true, burstcount=1, waitrequest=false → sFill
      dut.io.avalon.read.expect(true.B)
      dut.io.avalon.address.expect(0x1000.U)
      dut.io.avalon.burstcount.expect(1.U)
      dut.clock.step(1)

      // Cycle 2: sFill. Provide readdatavalid.
      dut.io.avalon.readdatavalid.poke(true.B)
      dut.io.avalon.readdata.poke(0xAB.U)
      dut.clock.step(1)
      dut.io.avalon.readdatavalid.poke(false.B)

      // Cycle 3: back to sIdle. FIFO has data.
      dut.io.dataReady.expect(true.B)
      dut.io.weightData.expect(0xAB.U)

      // Dequeue the beat
      dut.io.dequeue.poke(true.B)
      dut.clock.step(1)
      dut.io.dequeue.poke(false.B)

      // FIFO now empty
      dut.io.dataReady.expect(false.B)

      // rowDone should be true (1 beat dequeued = burstLen)
      dut.io.rowDone.expect(true.B)
    }
  }

  it should "fetch a 2-beat burst (K=128, 2 tiles)" in {
    test(new WeightStreamer) { dut =>
      dut.io.baseAddr.poke(0x0.U)
      dut.io.dimK.poke(128.U)
      dut.io.rowIdx.poke(0.U)

      dut.io.avalon.waitrequest.poke(false.B)
      dut.io.avalon.readdatavalid.poke(false.B)
      dut.io.avalon.readdata.poke(0.U)
      dut.io.dequeue.poke(false.B)

      // startRow → sRead
      dut.io.startRow.poke(true.B)
      dut.clock.step(1)
      dut.io.startRow.poke(false.B)

      // sRead: burstcount=2, addr=0
      dut.io.avalon.read.expect(true.B)
      dut.io.avalon.address.expect(0.U)
      dut.io.avalon.burstcount.expect(2.U)
      dut.clock.step(1)

      // sFill: provide first beat
      dut.io.avalon.readdatavalid.poke(true.B)
      dut.io.avalon.readdata.poke(0xAA.U)
      dut.clock.step(1)

      // sFill: provide second beat
      dut.io.avalon.readdata.poke(0xBB.U)
      dut.clock.step(1)
      dut.io.avalon.readdatavalid.poke(false.B)

      // FIFO has 2 beats. Dequeue first.
      dut.io.dataReady.expect(true.B)
      dut.io.weightData.expect(0xAA.U)
      dut.io.dequeue.poke(true.B)
      dut.clock.step(1)

      // Dequeue second.
      dut.io.dataReady.expect(true.B)
      dut.io.weightData.expect(0xBB.U)
      dut.clock.step(1)
      dut.io.dequeue.poke(false.B)

      // FIFO empty, rowDone
      dut.io.dataReady.expect(false.B)
      dut.io.rowDone.expect(true.B)
    }
  }

  it should "handle waitrequest delays" in {
    test(new WeightStreamer) { dut =>
      dut.io.baseAddr.poke(0x2000.U)
      dut.io.dimK.poke(64.U)
      dut.io.rowIdx.poke(0.U)

      dut.io.avalon.waitrequest.poke(true.B)
      dut.io.avalon.readdatavalid.poke(false.B)
      dut.io.avalon.readdata.poke(0.U)
      dut.io.dequeue.poke(false.B)

      // startRow
      dut.io.startRow.poke(true.B)
      dut.clock.step(1)
      dut.io.startRow.poke(false.B)

      // sRead with waitrequest=true — stays in sRead
      dut.io.avalon.read.expect(true.B)
      dut.clock.step(1)
      dut.io.avalon.read.expect(true.B)
      dut.clock.step(1)
      dut.io.avalon.read.expect(true.B)

      // Release waitrequest
      dut.io.avalon.waitrequest.poke(false.B)
      dut.clock.step(1)

      // Now in sFill — provide data
      dut.io.avalon.readdatavalid.poke(true.B)
      dut.io.avalon.readdata.poke(0xCD.U)
      dut.clock.step(1)
      dut.io.avalon.readdatavalid.poke(false.B)

      // FIFO has data
      dut.io.dataReady.expect(true.B)
      dut.io.weightData.expect(0xCD.U)
    }
  }

  it should "compute correct address for row 1 (K=128)" in {
    test(new WeightStreamer) { dut =>
      // K=128 → tilesPerRow=2, bytesPerBeat=16
      // Row 1: addr = 0x0 + 1*2*16 = 32 = 0x20
      dut.io.baseAddr.poke(0x0.U)
      dut.io.dimK.poke(128.U)
      dut.io.rowIdx.poke(1.U)

      dut.io.avalon.waitrequest.poke(false.B)
      dut.io.avalon.readdatavalid.poke(false.B)
      dut.io.avalon.readdata.poke(0.U)
      dut.io.dequeue.poke(false.B)

      dut.io.startRow.poke(true.B)
      dut.clock.step(1)
      dut.io.startRow.poke(false.B)

      // Check address and burstcount
      dut.io.avalon.read.expect(true.B)
      dut.io.avalon.address.expect(32.U)
      dut.io.avalon.burstcount.expect(2.U)
    }
  }

  it should "interleave burst fill and consumer dequeue" in {
    test(new WeightStreamer) { dut =>
      dut.io.baseAddr.poke(0x0.U)
      dut.io.dimK.poke(256.U)  // 4 tiles
      dut.io.rowIdx.poke(0.U)

      dut.io.avalon.waitrequest.poke(false.B)
      dut.io.avalon.readdatavalid.poke(false.B)
      dut.io.avalon.readdata.poke(0.U)
      dut.io.dequeue.poke(false.B)

      // startRow
      dut.io.startRow.poke(true.B)
      dut.clock.step(1)
      dut.io.startRow.poke(false.B)

      // sRead: burstcount=4
      dut.io.avalon.burstcount.expect(4.U)
      dut.clock.step(1)

      // Provide 4 back-to-back beats
      for (i <- 0 until 4) {
        dut.io.avalon.readdatavalid.poke(true.B)
        dut.io.avalon.readdata.poke((0x10 + i).U)
        dut.clock.step(1)
      }
      dut.io.avalon.readdatavalid.poke(false.B)

      // Dequeue all 4 in order
      for (i <- 0 until 4) {
        dut.io.dataReady.expect(true.B)
        dut.io.weightData.expect((0x10 + i).U)
        dut.io.dequeue.poke(true.B)
        dut.clock.step(1)
      }
      dut.io.dequeue.poke(false.B)

      dut.io.dataReady.expect(false.B)
      dut.io.rowDone.expect(true.B)
    }
  }

  it should "handle back-to-back row bursts" in {
    test(new WeightStreamer) { dut =>
      dut.io.baseAddr.poke(0x0.U)
      dut.io.dimK.poke(64.U)

      dut.io.avalon.waitrequest.poke(false.B)
      dut.io.avalon.readdatavalid.poke(false.B)
      dut.io.avalon.readdata.poke(0.U)
      dut.io.dequeue.poke(false.B)

      // Row 0 burst
      dut.io.rowIdx.poke(0.U)
      dut.io.startRow.poke(true.B)
      dut.clock.step(1)
      dut.io.startRow.poke(false.B)

      dut.io.avalon.address.expect(0.U)
      dut.clock.step(1)  // sRead → sFill

      dut.io.avalon.readdatavalid.poke(true.B)
      dut.io.avalon.readdata.poke(0xFF.U)
      dut.clock.step(1)
      dut.io.avalon.readdatavalid.poke(false.B)

      // Dequeue row 0
      dut.io.dequeue.poke(true.B)
      dut.clock.step(1)
      dut.io.dequeue.poke(false.B)

      // Row 1 burst
      dut.io.rowIdx.poke(1.U)
      dut.io.startRow.poke(true.B)
      dut.clock.step(1)
      dut.io.startRow.poke(false.B)

      // bytesPerBeat=16, tilesPerRow=1 → row 1 addr = 0 + 1*1*16 = 16
      dut.io.avalon.address.expect(16.U)
      dut.clock.step(1)

      dut.io.avalon.readdatavalid.poke(true.B)
      dut.io.avalon.readdata.poke(0xAA.U)
      dut.clock.step(1)
      dut.io.avalon.readdatavalid.poke(false.B)

      dut.io.dataReady.expect(true.B)
      dut.io.weightData.expect(0xAA.U)
    }
  }
}
