package bitnet

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class BitNetAcceleratorTest extends AnyFlatSpec with ChiselScalatestTester {
  // Use small config for testing
  implicit val cfg: BitNetConfig = BitNetConfig(
    numPEs = 4,
    maxDimM = 16,
    maxDimK = 16,
    avalonDataW = 8   // 4 PEs * 2 bits = 8 bits
  )

  behavior of "BitNetAccelerator"

  /** Helper to write a register via Avalon slave */
  def writeReg(dut: BitNetAccelerator, addr: Int, data: BigInt): Unit = {
    dut.io.slave.address.poke(addr.U)
    dut.io.slave.writedata.poke(data.U)
    dut.io.slave.write.poke(true.B)
    dut.io.slave.read.poke(false.B)
    dut.clock.step(1)
    dut.io.slave.write.poke(false.B)
  }

  /** Helper to read a register via Avalon slave */
  def readReg(dut: BitNetAccelerator, addr: Int): BigInt = {
    dut.io.slave.address.poke(addr.U)
    dut.io.slave.read.poke(true.B)
    dut.io.slave.write.poke(false.B)
    dut.clock.step(1)
    val value = dut.io.slave.readdata.peek().litValue
    dut.io.slave.read.poke(false.B)
    value
  }

  it should "accept register writes and reads" in {
    test(new BitNetAccelerator) { dut =>
      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      // Write weight base address
      writeReg(dut, 0x08, 0x10000)
      val base = readReg(dut, 0x08)
      assert(base == 0x10000, s"Expected 0x10000, got $base")

      // Write dimensions
      writeReg(dut, 0x0C, 4)  // M = 4
      writeReg(dut, 0x10, 4)  // K = 4
      assert(readReg(dut, 0x0C) == 4)
      assert(readReg(dut, 0x10) == 4)

      // Write shift amount
      writeReg(dut, 0x14, 2)
      assert(readReg(dut, 0x14) == 2)
    }
  }

  it should "write activations and read results" in {
    test(new BitNetAccelerator) { dut =>
      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      // Write activations at 0x80+
      for (i <- 0 until 4) {
        writeReg(dut, 0x80 + i * 4, i + 1)
      }

      // Status should show not busy
      val status = readReg(dut, 0x04)
      assert((status & 1) == 0, "Should not be busy initially")
    }
  }

  it should "report busy status after start" in {
    test(new BitNetAccelerator) { dut =>
      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      // Configure
      writeReg(dut, 0x0C, 1)  // M = 1
      writeReg(dut, 0x10, 4)  // K = 4
      writeReg(dut, 0x14, 0)  // shift = 0

      // Start
      writeReg(dut, 0x00, 1)

      // Check busy
      dut.clock.step(2)
      val status = readReg(dut, 0x04)
      assert((status & 1) == 1, "Should be busy after start")
    }
  }
}
