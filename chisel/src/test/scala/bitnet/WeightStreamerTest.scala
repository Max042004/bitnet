package bitnet

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class WeightStreamerTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val cfg: BitNetConfig = BitNetConfig()

  behavior of "WeightStreamer"

  it should "fetch a single tile with no waitrequest" in {
    test(new WeightStreamer) { dut =>
      dut.io.baseAddr.poke(0x1000.U)
      dut.io.dimK.poke(64.U)
      dut.io.rowIdx.poke(0.U)
      dut.io.tileIdx.poke(0.U)

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

      // Cycle 2: state=sWaitData. Provide readdatavalid.
      dut.io.avalon.readdatavalid.poke(true.B)
      dut.io.avalon.readdata.poke(0xAB.U)
      dut.io.weightValid.expect(true.B)
      dut.io.weightData.expect(0xAB.U)
      dut.clock.step(1)
      dut.io.avalon.readdatavalid.poke(false.B)

      // Cycle 3: back to sIdle
      dut.io.weightValid.expect(false.B)
    }
  }

  it should "handle waitrequest delays" in {
    test(new WeightStreamer) { dut =>
      dut.io.baseAddr.poke(0x2000.U)
      dut.io.dimK.poke(64.U)
      dut.io.rowIdx.poke(0.U)
      dut.io.tileIdx.poke(0.U)

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
      dut.io.weightValid.expect(true.B)
      dut.clock.step(1)
      dut.io.avalon.readdatavalid.poke(false.B)

      // Back to sIdle
      dut.io.weightValid.expect(false.B)
    }
  }

  it should "compute correct address for row 1 tile 0" in {
    test(new WeightStreamer) { dut =>
      // K=128 → tilesPerRow=2, bytesPerBeat=16
      // Row 1, Tile 0: addr = 0x0 + (1*2 + 0)*16 = 32 = 0x20
      dut.io.baseAddr.poke(0x0.U)
      dut.io.dimK.poke(128.U)
      dut.io.rowIdx.poke(1.U)
      dut.io.tileIdx.poke(0.U)

      dut.io.avalon.waitrequest.poke(false.B)
      dut.io.avalon.readdatavalid.poke(false.B)
      dut.io.avalon.readdata.poke(0.U)

      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      // Check address: (1*2 + 0) * 16 = 32
      dut.io.avalon.read.expect(true.B)
      dut.io.avalon.address.expect(32.U)
    }
  }

  it should "compute correct address for row 1 tile 1" in {
    test(new WeightStreamer) { dut =>
      // K=128 → tilesPerRow=2, bytesPerBeat=16
      // Row 1, Tile 1: addr = 0x0 + (1*2 + 1)*16 = 48 = 0x30
      dut.io.baseAddr.poke(0x0.U)
      dut.io.dimK.poke(128.U)
      dut.io.rowIdx.poke(1.U)
      dut.io.tileIdx.poke(1.U)

      dut.io.avalon.waitrequest.poke(false.B)
      dut.io.avalon.readdatavalid.poke(false.B)
      dut.io.avalon.readdata.poke(0.U)

      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      dut.io.avalon.read.expect(true.B)
      dut.io.avalon.address.expect(48.U)
    }
  }

  it should "handle back-to-back tile fetches" in {
    test(new WeightStreamer) { dut =>
      dut.io.baseAddr.poke(0x0.U)
      dut.io.dimK.poke(128.U)

      dut.io.avalon.waitrequest.poke(false.B)
      dut.io.avalon.readdatavalid.poke(false.B)
      dut.io.avalon.readdata.poke(0.U)

      // Fetch tile 0
      dut.io.rowIdx.poke(0.U)
      dut.io.tileIdx.poke(0.U)
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      // sRead
      dut.io.avalon.read.expect(true.B)
      dut.io.avalon.address.expect(0.U)
      dut.clock.step(1)

      // sWaitData → provide data
      dut.io.avalon.readdatavalid.poke(true.B)
      dut.io.avalon.readdata.poke(0xFF.U)
      dut.io.weightValid.expect(true.B)
      dut.io.weightData.expect(0xFF.U)
      dut.clock.step(1)
      dut.io.avalon.readdatavalid.poke(false.B)

      // Back to sIdle — now fetch tile 1
      dut.io.rowIdx.poke(0.U)
      dut.io.tileIdx.poke(1.U)
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      // sRead for tile 1
      dut.io.avalon.read.expect(true.B)
      dut.io.avalon.address.expect(16.U)  // bytesPerBeat = 16
      dut.clock.step(1)

      // sWaitData → provide data
      dut.io.avalon.readdatavalid.poke(true.B)
      dut.io.avalon.readdata.poke(0xAA.U)
      dut.io.weightValid.expect(true.B)
      dut.io.weightData.expect(0xAA.U)
      dut.clock.step(1)
      dut.io.avalon.readdatavalid.poke(false.B)

      // Back to sIdle
      dut.io.weightValid.expect(false.B)
    }
  }
}
