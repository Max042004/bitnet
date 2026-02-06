package bitnet

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class WeightStreamerTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val cfg: BitNetConfig = BitNetConfig()

  behavior of "WeightStreamer"

  it should "stream weights for a single row with no waitrequest" in {
    test(new WeightStreamer) { dut =>
      dut.io.baseAddr.poke(0x1000.U)
      dut.io.dimM.poke(1.U)
      dut.io.dimK.poke(64.U)

      dut.io.avalon.waitrequest.poke(false.B)
      dut.io.avalon.readdatavalid.poke(false.B)
      dut.io.avalon.readdata.poke(0.U)

      // Cycle 0: start pulse. State: sIdle → sRead
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      // Cycle 1: state=sRead. read=true, waitrequest=false → sWaitData
      dut.io.avalon.read.expect(true.B)
      dut.io.avalon.address.expect(0x1000.U)
      dut.clock.step(1)

      // Cycle 2: state=sWaitData. Provide readdatavalid this cycle.
      dut.io.avalon.readdatavalid.poke(true.B)
      dut.io.avalon.readdata.poke(0xAB.U)
      dut.clock.step(1)
      dut.io.avalon.readdatavalid.poke(false.B)

      // Cycle 3: state=sRowDone (since 1 tile, nextTile=1 >= tilesPerRow=1)
      dut.io.rowDone.expect(true.B)
      dut.clock.step(1)

      // Cycle 4: state=sDone (since 1 row, nextRow=1 >= totalRows=1)
      dut.io.allDone.expect(true.B)
    }
  }

  it should "handle waitrequest delays" in {
    test(new WeightStreamer) { dut =>
      dut.io.baseAddr.poke(0x2000.U)
      dut.io.dimM.poke(1.U)
      dut.io.dimK.poke(64.U)

      dut.io.avalon.waitrequest.poke(true.B)
      dut.io.avalon.readdatavalid.poke(false.B)
      dut.io.avalon.readdata.poke(0.U)

      // Start
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      // State=sRead, but waitrequest=true, so stays in sRead
      dut.io.avalon.read.expect(true.B)
      dut.clock.step(1)
      dut.io.avalon.read.expect(true.B)
      dut.clock.step(1)
      dut.io.avalon.read.expect(true.B)

      // Release waitrequest
      dut.io.avalon.waitrequest.poke(false.B)
      dut.clock.step(1)

      // Now in sWaitData
      dut.io.avalon.readdatavalid.poke(true.B)
      dut.clock.step(1)
      dut.io.avalon.readdatavalid.poke(false.B)

      // sRowDone
      dut.io.rowDone.expect(true.B)
    }
  }

  it should "stream multiple tiles for K > numPEs" in {
    test(new WeightStreamer) { dut =>
      dut.io.baseAddr.poke(0x0.U)
      dut.io.dimM.poke(1.U)
      dut.io.dimK.poke(128.U)  // 2 tiles

      dut.io.avalon.waitrequest.poke(false.B)
      dut.io.avalon.readdatavalid.poke(false.B)
      dut.io.avalon.readdata.poke(0.U)

      // Start → sRead
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      // sRead for tile 0, waitrequest=false → sWaitData
      dut.io.avalon.read.expect(true.B)
      dut.io.avalon.address.expect(0x0.U)
      dut.clock.step(1)

      // sWaitData, provide data for tile 0
      dut.io.avalon.readdatavalid.poke(true.B)
      dut.clock.step(1)
      dut.io.avalon.readdatavalid.poke(false.B)

      // nextTile=1 < tilesPerRow=2, so → sRead for tile 1
      dut.io.avalon.read.expect(true.B)
      val bytesPerBeat = cfg.avalonDataW / 8
      dut.io.avalon.address.expect(bytesPerBeat.U)
      dut.clock.step(1)

      // sWaitData for tile 1
      dut.io.avalon.readdatavalid.poke(true.B)
      dut.clock.step(1)
      dut.io.avalon.readdatavalid.poke(false.B)

      // nextTile=2 >= tilesPerRow=2, so → sRowDone
      dut.io.rowDone.expect(true.B)
      dut.clock.step(1)

      // sDone (1 row)
      dut.io.allDone.expect(true.B)
    }
  }

  it should "stream multiple rows" in {
    test(new WeightStreamer) { dut =>
      dut.io.baseAddr.poke(0x0.U)
      dut.io.dimM.poke(2.U)
      dut.io.dimK.poke(64.U)  // 1 tile per row

      dut.io.avalon.waitrequest.poke(false.B)
      dut.io.avalon.readdatavalid.poke(false.B)
      dut.io.avalon.readdata.poke(0.U)

      // Start → sRead
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      // Row 0: sRead → sWaitData
      dut.clock.step(1)

      // sWaitData, provide data
      dut.io.avalon.readdatavalid.poke(true.B)
      dut.clock.step(1)
      dut.io.avalon.readdatavalid.poke(false.B)

      // sRowDone
      dut.io.rowDone.expect(true.B)
      dut.io.rowIdx.expect(0.U)
      dut.clock.step(1)

      // Row 1: sRead → sWaitData
      dut.clock.step(1)

      dut.io.avalon.readdatavalid.poke(true.B)
      dut.clock.step(1)
      dut.io.avalon.readdatavalid.poke(false.B)

      // sRowDone
      dut.io.rowDone.expect(true.B)
      dut.io.rowIdx.expect(1.U)
      dut.clock.step(1)

      // sDone
      dut.io.allDone.expect(true.B)
    }
  }
}
