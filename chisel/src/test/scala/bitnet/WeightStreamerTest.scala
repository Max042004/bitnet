package bitnet

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class WeightStreamerTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val cfg: BitNetConfig = BitNetConfig(numPEs = 64, avalonDataW = 128)

  /** Helper: fill the fill-side FIFO, then swap to make it active.
    * This replaces the old single-FIFO startRow pattern.
    */
  def fillAndSwap(dut: WeightStreamer, baseAddr: Int, dimK: Int, rowIdx: Int, beats: Seq[BigInt]): Unit = {
    dut.io.baseAddr.poke(baseAddr.U)
    dut.io.dimK.poke(dimK.U)
    dut.io.rowIdx.poke(rowIdx.U)

    // Start filling the fill-side FIFO
    dut.io.startRow.poke(true.B)
    dut.clock.step(1)
    dut.io.startRow.poke(false.B)

    // sRead cycle
    dut.clock.step(1)

    // sFill: provide beats
    for (beat <- beats) {
      dut.io.avalon.readdatavalid.poke(true.B)
      dut.io.avalon.readdata.poke(beat.U)
      dut.clock.step(1)
    }
    dut.io.avalon.readdatavalid.poke(false.B)

    // prefetchDone should be true
    dut.io.prefetchDone.expect(true.B)

    // Swap to make the filled FIFO the active one
    dut.io.swap.poke(true.B)
    dut.clock.step(1)
    dut.io.swap.poke(false.B)
  }

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
      dut.io.swap.poke(false.B)

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

      // prefetchDone should be true
      dut.io.prefetchDone.expect(true.B)

      // Swap to make fill FIFO active
      dut.io.swap.poke(true.B)
      dut.clock.step(1)
      dut.io.swap.poke(false.B)

      // Active FIFO has data
      dut.io.dataReady.expect(true.B)
      dut.io.weightData.expect(0xAB.U)

      // Dequeue the beat
      dut.io.dequeue.poke(true.B)
      dut.clock.step(1)
      dut.io.dequeue.poke(false.B)

      // FIFO now empty
      dut.io.dataReady.expect(false.B)

      // rowDone should be true (1 beat dequeued = activeBurstLen)
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
      dut.io.swap.poke(false.B)

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

      // Swap to make filled FIFO active
      dut.io.swap.poke(true.B)
      dut.clock.step(1)
      dut.io.swap.poke(false.B)

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
      dut.io.swap.poke(false.B)

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

      // Swap to access data
      dut.io.swap.poke(true.B)
      dut.clock.step(1)
      dut.io.swap.poke(false.B)

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
      dut.io.swap.poke(false.B)

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
      dut.io.swap.poke(false.B)

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

      // Swap to make data available
      dut.io.swap.poke(true.B)
      dut.clock.step(1)
      dut.io.swap.poke(false.B)

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
      dut.io.swap.poke(false.B)

      // Row 0: fill → swap → dequeue
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

      // Swap to activate row 0 data
      dut.io.swap.poke(true.B)
      dut.clock.step(1)
      dut.io.swap.poke(false.B)

      // Dequeue row 0
      dut.io.dequeue.poke(true.B)
      dut.clock.step(1)
      dut.io.dequeue.poke(false.B)

      // Row 1: fill → swap → verify
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

      // Swap to activate row 1 data
      dut.io.swap.poke(true.B)
      dut.clock.step(1)
      dut.io.swap.poke(false.B)

      dut.io.dataReady.expect(true.B)
      dut.io.weightData.expect(0xAA.U)
    }
  }

  // ---- Double-buffer specific tests ----

  it should "prefetch next row while consuming current row" in {
    test(new WeightStreamer) { dut =>
      dut.io.avalon.waitrequest.poke(false.B)
      dut.io.avalon.readdatavalid.poke(false.B)
      dut.io.avalon.readdata.poke(0.U)
      dut.io.dequeue.poke(false.B)
      dut.io.swap.poke(false.B)

      // Fill row 0 into fill FIFO
      fillAndSwap(dut, 0x0, 64, 0, Seq(BigInt(0xAA)))

      // Now start prefetching row 1 into the (now empty) fill FIFO
      dut.io.rowIdx.poke(1.U)
      dut.io.startRow.poke(true.B)
      dut.clock.step(1)
      dut.io.startRow.poke(false.B)

      // Simultaneously dequeue row 0 from active FIFO
      dut.io.dataReady.expect(true.B)
      dut.io.weightData.expect(0xAA.U)
      dut.io.dequeue.poke(true.B)

      // sRead for row 1 fill
      dut.clock.step(1)
      dut.io.dequeue.poke(false.B)

      // Provide row 1 data
      dut.io.avalon.readdatavalid.poke(true.B)
      dut.io.avalon.readdata.poke(0xBB.U)
      dut.clock.step(1)
      dut.io.avalon.readdatavalid.poke(false.B)

      // Row 0 consumed, row 1 prefetched
      dut.io.rowDone.expect(true.B)
      dut.io.prefetchDone.expect(true.B)

      // Swap to row 1
      dut.io.swap.poke(true.B)
      dut.clock.step(1)
      dut.io.swap.poke(false.B)

      // Verify row 1 data
      dut.io.dataReady.expect(true.B)
      dut.io.weightData.expect(0xBB.U)
    }
  }

  it should "swap toggles active FIFO correctly across 3 rows" in {
    test(new WeightStreamer) { dut =>
      dut.io.avalon.waitrequest.poke(false.B)
      dut.io.avalon.readdatavalid.poke(false.B)
      dut.io.avalon.readdata.poke(0.U)
      dut.io.dequeue.poke(false.B)
      dut.io.swap.poke(false.B)

      val rowData = Seq(BigInt(0x11), BigInt(0x22), BigInt(0x33))
      for (row <- 0 until 3) {
        fillAndSwap(dut, 0x0, 64, row, Seq(rowData(row)))

        // Verify data from active FIFO
        dut.io.dataReady.expect(true.B)
        dut.io.weightData.expect(rowData(row).U)
        dut.io.dequeue.poke(true.B)
        dut.clock.step(1)
        dut.io.dequeue.poke(false.B)

        dut.io.rowDone.expect(true.B)
      }
    }
  }

  it should "track prefetchDone correctly" in {
    test(new WeightStreamer) { dut =>
      dut.io.avalon.waitrequest.poke(false.B)
      dut.io.avalon.readdatavalid.poke(false.B)
      dut.io.avalon.readdata.poke(0.U)
      dut.io.dequeue.poke(false.B)
      dut.io.swap.poke(false.B)

      // Initially no prefetch done
      dut.io.prefetchDone.expect(false.B)

      // Start row 0 fill
      dut.io.baseAddr.poke(0x0.U)
      dut.io.dimK.poke(128.U)  // 2 beats
      dut.io.rowIdx.poke(0.U)
      dut.io.startRow.poke(true.B)
      dut.clock.step(1)
      dut.io.startRow.poke(false.B)

      // prefetchDone should be false during fill
      dut.io.prefetchDone.expect(false.B)

      // sRead
      dut.clock.step(1)

      // First beat
      dut.io.avalon.readdatavalid.poke(true.B)
      dut.io.avalon.readdata.poke(0xAA.U)
      dut.clock.step(1)
      dut.io.prefetchDone.expect(false.B)  // Not done yet

      // Second beat
      dut.io.avalon.readdata.poke(0xBB.U)
      dut.clock.step(1)
      dut.io.avalon.readdatavalid.poke(false.B)

      // Now done
      dut.io.prefetchDone.expect(true.B)

      // After swap, prefetchDone resets
      dut.io.swap.poke(true.B)
      dut.clock.step(1)
      dut.io.swap.poke(false.B)
      dut.io.prefetchDone.expect(false.B)
    }
  }
}
