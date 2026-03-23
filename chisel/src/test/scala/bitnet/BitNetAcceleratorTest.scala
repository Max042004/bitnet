package bitnet

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class BitNetAcceleratorTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val cfg: BitNetConfig = BitNetConfig(
    numPEs = 4,
    maxDimM = 16,
    maxDimK = 16,
    avalonDataW = 8
  )

  behavior of "BitNetAccelerator"

  def writeReg(dut: BitNetAccelerator, addr: Int, data: BigInt): Unit = {
    dut.io.slave.address.poke(addr.U)
    dut.io.slave.writedata.poke(data.U)
    dut.io.slave.write.poke(true.B)
    dut.io.slave.read.poke(false.B)
    dut.clock.step(1)
    dut.io.slave.write.poke(false.B)
  }

  def readReg(dut: BitNetAccelerator, addr: Int): BigInt = {
    dut.io.slave.address.poke(addr.U)
    dut.io.slave.read.poke(true.B)
    dut.io.slave.write.poke(false.B)
    dut.clock.step(1)
    val value = dut.io.slave.readdata.peek().litValue
    dut.io.slave.read.poke(false.B)
    value
  }

  def packWeights(weights: Seq[Int]): BigInt = {
    var packed = BigInt(0)
    for (i <- weights.indices) {
      val enc = weights(i) match {
        case 0 => 0; case 1 => 1; case -1 => 2
      }
      packed = packed | (BigInt(enc) << (i * 2))
    }
    packed
  }

  /** Step cycles, providing weight data whenever master.read is asserted.
    * Returns number of cycles elapsed.
    */
  def stepWithWeights(dut: BitNetAccelerator, weightData: Seq[BigInt], cycles: Int): Int = {
    val weightQueue = scala.collection.mutable.Queue(weightData: _*)
    var elapsed = 0

    for (_ <- 0 until cycles) {
      // Check if master wants to read and we have data
      if (dut.io.master.read.peek().litToBoolean && weightQueue.nonEmpty) {
        // Burst already accepted (waitrequest=false), provide data next cycle
        dut.clock.step(1)
        elapsed += 1
        // Provide consecutive beats
        while (weightQueue.nonEmpty && elapsed < cycles) {
          dut.io.master.readdatavalid.poke(true.B)
          dut.io.master.readdata.poke(weightQueue.dequeue().U)
          dut.clock.step(1)
          elapsed += 1
        }
        dut.io.master.readdatavalid.poke(false.B)
      } else {
        dut.clock.step(1)
        elapsed += 1
      }
    }
    elapsed
  }

  it should "complete a simple 1x4 dot product via MMIO" in {
    test(new BitNetAccelerator).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)
      dut.io.slave.read.poke(false.B)
      dut.io.slave.write.poke(false.B)

      // M=1, K=4: activations [1,1,1,1], weights all +1 → 4
      for (i <- 0 until 4) writeReg(dut, 0x80 + i * 4, 1)
      writeReg(dut, 0x08, 0x1000)
      writeReg(dut, 0x0C, 1)
      writeReg(dut, 0x10, 4)
      // TILE_STRIDE = M * tileBytesSize = 1 * 1 = 1
      writeReg(dut, 0x20, 1)
      writeReg(dut, 0x00, 1)

      // Run for enough cycles, providing weights
      val weights = Seq(packWeights(Seq(1, 1, 1, 1)))
      stepWithWeights(dut, weights, 100)

      // Check status
      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"FSM not done, status=$status")

      val result = readReg(dut, 0x8000)
      println(s"Result[0] = $result (expected: 4)")
      assert(result == 4, s"Expected 4, got $result")
    }
  }

  it should "compute 2x4 matrix-vector product" in {
    test(new BitNetAccelerator).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)
      dut.io.slave.read.poke(false.B)
      dut.io.slave.write.poke(false.B)

      // M=2, K=4: activations [1,2,3,4]
      // Row 0: [+1,+1,+1,+1] → 10
      // Row 1: [+1,-1,+1,-1] → -2
      // Tile-major: [tile0_row0, tile0_row1]
      val acts = Seq(1, 2, 3, 4)
      for (i <- 0 until 4) writeReg(dut, 0x80 + i * 4, acts(i))
      writeReg(dut, 0x08, 0x1000)
      writeReg(dut, 0x0C, 2)
      writeReg(dut, 0x10, 4)
      // TILE_STRIDE = M * tileBytesSize = 2 * 1 = 2
      writeReg(dut, 0x20, 2)
      writeReg(dut, 0x00, 1)

      val row0w = packWeights(Seq(1, 1, 1, 1))
      val row1w = packWeights(Seq(1, -1, 1, -1))
      stepWithWeights(dut, Seq(row0w, row1w), 200)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"FSM not done, status=$status")

      val r0 = readReg(dut, 0x8000)
      val r1raw = readReg(dut, 0x8004)
      val r1 = if (r1raw >= (BigInt(1) << 31)) (r1raw - (BigInt(1) << 32)).toInt else r1raw.toInt
      println(s"Result[0] = $r0 (expected: 10)")
      println(s"Result[1] = $r1 (expected: -2)")
      assert(r0 == 10, s"Row 0: expected 10, got $r0")
      assert(r1 == -2, s"Row 1: expected -2, got $r1")
    }
  }
}
