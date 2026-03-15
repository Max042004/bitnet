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

  /** Pack weight values for 4 PEs into an 8-bit word.
    * Each weight: 0→00, +1→01, -1→10
    */
  def packWeights(weights: Seq[Int]): BigInt = {
    var packed = BigInt(0)
    for (i <- weights.indices) {
      val enc = weights(i) match {
        case 0  => 0
        case 1  => 1
        case -1 => 2
      }
      packed = packed | (BigInt(enc) << (i * 2))
    }
    packed
  }

  /** Simulate accelerator with DDR3 burst memory model responding to Avalon master reads.
    * Captures address + burstcount when read is asserted, then delivers
    * back-to-back beats with 1-cycle initial latency.
    * Also handles Avalon master writes (stores written data into ddr3Mem).
    */
  def runWithMemory(dut: BitNetAccelerator, weightMem: Map[Int, BigInt], cycles: Int,
                    ddr3Mem: scala.collection.mutable.Map[Int, BigInt] = null): Unit = {
    val bytesPerBeat = cfg.avalonDataW / 8
    var burstAddr = 0
    var burstRemaining = 0
    var newBurstPending = false
    var newBurstAddr = 0
    var newBurstLen = 0

    // Merge weightMem into ddr3Mem if provided
    if (ddr3Mem != null) {
      weightMem.foreach { case (k, v) => ddr3Mem(k) = v }
    }

    for (_ <- 0 until cycles) {
      // Start delivering a burst captured last cycle
      if (newBurstPending) {
        burstAddr = newBurstAddr
        burstRemaining = newBurstLen
        newBurstPending = false
      }

      // Deliver burst data
      if (burstRemaining > 0) {
        dut.io.master.readdatavalid.poke(true.B)
        val data = if (ddr3Mem != null) {
          ddr3Mem.getOrElse(burstAddr, BigInt(0))
        } else {
          weightMem.getOrElse(burstAddr, BigInt(0))
        }
        dut.io.master.readdata.poke(data.U)
        burstAddr += bytesPerBeat
        burstRemaining -= 1
      } else {
        dut.io.master.readdatavalid.poke(false.B)
      }

      // Capture new burst read request
      if (dut.io.master.read.peek().litToBoolean) {
        newBurstAddr = dut.io.master.address.peek().litValue.toInt
        newBurstLen = dut.io.master.burstcount.peek().litValue.toInt
        newBurstPending = true
      }

      // Handle write transactions
      if (ddr3Mem != null && dut.io.master.write.peek().litToBoolean) {
        val writeAddr = dut.io.master.address.peek().litValue.toInt
        val writeData = dut.io.master.writedata.peek().litValue
        ddr3Mem(writeAddr) = writeData
      }

      dut.clock.step(1)
    }
  }

  /** Read raw 32-bit accumulator result from result buffer, sign-extending to Int */
  def readResult(dut: BitNetAccelerator, row: Int): Int = {
    val raw = readReg(dut, 0x8000 + row * 4)
    if (raw > 0x7FFFFFFFL) (raw - 0x100000000L).toInt else raw.toInt
  }

  /** Write an INT8 activation value (handles negative values via unsigned conversion) */
  def writeAct(dut: BitNetAccelerator, idx: Int, value: Int): Unit = {
    writeReg(dut, 0x80 + idx * 4, BigInt(value & 0xFF))
  }

  /** Software reference model: raw dot product (no shift/clamp — FPGA outputs raw accumulator) */
  def refModel(weights: Seq[Int], acts: Seq[Int]): Int = {
    weights.zip(acts).map { case (w, a) => w * a }.sum
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

  // ---- Integration tests: end-to-end multi-row computation ----

  it should "compute M=2 multi-row with opposite weight signs" in {
    test(new BitNetAccelerator) { dut =>
      // M=2, K=4, shift=0. Activations=[1,1,1,1]
      // Row 0: all +1 weights → 4.  Row 1: all -1 weights → -4.
      val weightBase = 0x1000
      // bytesPerBeat=1, tilesPerRow=1
      // row 0 addr: 0x1000 + (0*1+0)*1 = 0x1000
      // row 1 addr: 0x1000 + (1*1+0)*1 = 0x1001
      val weightMem = Map(
        0x1000 -> packWeights(Seq(1, 1, 1, 1)),   // 0x55
        0x1001 -> packWeights(Seq(-1, -1, -1, -1)) // 0xAA
      )

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      for (i <- 0 until 4) writeReg(dut, 0x80 + i * 4, 1)
      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x0C, 2)  // M
      writeReg(dut, 0x10, 4)  // K
      writeReg(dut, 0x14, 0)  // shift
      writeReg(dut, 0x00, 1)  // START

      runWithMemory(dut, weightMem, 200)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val r0 = readResult(dut, 0)
      val r1 = readResult(dut, 1)
      assert(r0 == 4, s"Row 0: expected 4, got $r0")
      assert(r1 == -4, s"Row 1: expected -4, got $r1")
    }
  }

  it should "compute M=4 multi-row with varying weight patterns" in {
    test(new BitNetAccelerator) { dut =>
      // M=4, K=4, shift=0. Activations=[1,2,3,4]
      // Row 0: all +1    → 1+2+3+4 = 10
      // Row 1: all  0    → 0
      // Row 2: all -1    → -(1+2+3+4) = -10
      // Row 3: +1,-1,+1,-1 → 1-2+3-4 = -2
      val weightBase = 0x2000
      val weightMem = Map(
        0x2000 -> packWeights(Seq(1, 1, 1, 1)),
        0x2001 -> packWeights(Seq(0, 0, 0, 0)),
        0x2002 -> packWeights(Seq(-1, -1, -1, -1)),
        0x2003 -> packWeights(Seq(1, -1, 1, -1))
      )

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      val acts = Seq(1, 2, 3, 4)
      for (i <- acts.indices) writeReg(dut, 0x80 + i * 4, acts(i))
      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x0C, 4)  // M
      writeReg(dut, 0x10, 4)  // K
      writeReg(dut, 0x14, 0)  // shift
      writeReg(dut, 0x00, 1)  // START

      runWithMemory(dut, weightMem, 300)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val expected = Seq(10, 0, -10, -2)
      for (row <- 0 until 4) {
        val result = readResult(dut, row)
        assert(result == expected(row),
          s"Row $row: expected ${expected(row)}, got $result")
      }
    }
  }

  it should "compute M=2 multi-row with K=8 (2 tiles per row)" in {
    test(new BitNetAccelerator) { dut =>
      // M=2, K=8, shift=0. Activations=[1,1,1,1,1,1,1,1]
      // tilesPerRow=2, bytesPerBeat=1
      // Row 0: all +1 → 8.  Row 1: all -1 → -8.
      // Addresses: base + (row*2 + tile)*1
      val weightBase = 0x3000
      val weightMem = Map(
        0x3000 -> packWeights(Seq(1, 1, 1, 1)),    // row 0 tile 0
        0x3001 -> packWeights(Seq(1, 1, 1, 1)),    // row 0 tile 1
        0x3002 -> packWeights(Seq(-1, -1, -1, -1)), // row 1 tile 0
        0x3003 -> packWeights(Seq(-1, -1, -1, -1))  // row 1 tile 1
      )

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      for (i <- 0 until 8) writeReg(dut, 0x80 + i * 4, 1)
      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x0C, 2)  // M
      writeReg(dut, 0x10, 8)  // K
      writeReg(dut, 0x14, 0)  // shift
      writeReg(dut, 0x00, 1)  // START

      runWithMemory(dut, weightMem, 300)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val r0 = readResult(dut, 0)
      val r1 = readResult(dut, 1)
      assert(r0 == 8, s"Row 0: expected 8, got $r0")
      assert(r1 == -8, s"Row 1: expected -8, got $r1")
    }
  }

  it should "compute M=1 with K=1024 (256 tiles, max-K deep accumulation)" in {
    // Uses a config with maxDimK=1024 to exercise the full activation address range.
    // Every 4th weight = +1, rest 0. All activations = 4.
    // Per tile: [+1,0,0,0] · [4,4,4,4] = 4.  256 tiles × 4 = 1024 (raw accumulator).
    val largeCfg = BitNetConfig(
      numPEs = 4,
      maxDimM = 16,
      maxDimK = 1024,
      avalonDataW = 8
    )
    test(new BitNetAccelerator()(largeCfg)) { dut =>
      dut.clock.setTimeout(0)
      val K = 1024
      val shift = 4
      val weightBase = 0x5000
      val tilesPerRow = (K + 3) / 4 // 256
      val weightPacked = packWeights(Seq(1, 0, 0, 0))
      val weightMem = (0 until tilesPerRow).map(t => (weightBase + t) -> weightPacked).toMap

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      for (i <- 0 until K) writeReg(dut, 0x80 + i * 4, 4)
      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x0C, 1)     // M
      writeReg(dut, 0x10, K)     // K
      writeReg(dut, 0x14, shift) // shift
      writeReg(dut, 0x00, 1)     // START

      runWithMemory(dut, weightMem, 5000)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val result = readResult(dut, 0)
      assert(result == 1024, s"Expected 1024, got $result")
    }
  }

  it should "compute M=2 multi-row (shift register ignored — raw accumulator output)" in {
    test(new BitNetAccelerator) { dut =>
      // M=2, K=4. Activations=[4,4,4,4]
      // Row 0: all +1 → sum=16 (raw accumulator)
      // Row 1: all -1 → sum=-16 (raw accumulator)
      val weightBase = 0x4000
      val weightMem = Map(
        0x4000 -> packWeights(Seq(1, 1, 1, 1)),
        0x4001 -> packWeights(Seq(-1, -1, -1, -1))
      )

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      for (i <- 0 until 4) writeReg(dut, 0x80 + i * 4, 4)
      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x0C, 2)  // M
      writeReg(dut, 0x10, 4)  // K
      writeReg(dut, 0x14, 2)  // shift (ignored)
      writeReg(dut, 0x00, 1)  // START

      runWithMemory(dut, weightMem, 200)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val r0 = readResult(dut, 0)
      val r1 = readResult(dut, 1)
      assert(r0 == 16, s"Row 0: expected 16, got $r0")
      assert(r1 == -16, s"Row 1: expected -16, got $r1")
    }
  }

  // ---- F: Negative activation tests ----

  it should "F1: compute with negative activations and all +1 weights" in {
    test(new BitNetAccelerator) { dut =>
      // M=1, K=4, shift=0. acts=[-2,-2,-2,-2], weights all +1 → -8
      val weightBase = 0x6000
      val weightMem = Map(0x6000 -> packWeights(Seq(1, 1, 1, 1)))

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      for (i <- 0 until 4) writeAct(dut, i, -2)
      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x0C, 1)  // M
      writeReg(dut, 0x10, 4)  // K
      writeReg(dut, 0x14, 0)  // shift
      writeReg(dut, 0x00, 1)  // START

      runWithMemory(dut, weightMem, 200)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val r0 = readResult(dut, 0)
      assert(r0 == -8, s"Expected -8, got $r0")
    }
  }

  it should "F2: double negation with negative activations and -1 weights" in {
    test(new BitNetAccelerator) { dut =>
      // M=1, K=4, shift=0. acts=[-2,-2,-2,-2], weights all -1 → 8
      val weightBase = 0x6100
      val weightMem = Map(0x6100 -> packWeights(Seq(-1, -1, -1, -1)))

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      for (i <- 0 until 4) writeAct(dut, i, -2)
      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x0C, 1)  // M
      writeReg(dut, 0x10, 4)  // K
      writeReg(dut, 0x14, 0)  // shift
      writeReg(dut, 0x00, 1)  // START

      runWithMemory(dut, weightMem, 200)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val r0 = readResult(dut, 0)
      assert(r0 == 8, s"Expected 8, got $r0")
    }
  }

  it should "F3: mixed positive/negative activations cancel to zero" in {
    test(new BitNetAccelerator) { dut =>
      // M=1, K=4, shift=0. acts=[3,3,-3,-3], weights all +1 → 0
      val weightBase = 0x6200
      val weightMem = Map(0x6200 -> packWeights(Seq(1, 1, 1, 1)))

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      val acts = Seq(3, 3, -3, -3)
      for (i <- acts.indices) writeAct(dut, i, acts(i))
      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x0C, 1)  // M
      writeReg(dut, 0x10, 4)  // K
      writeReg(dut, 0x14, 0)  // shift
      writeReg(dut, 0x00, 1)  // START

      runWithMemory(dut, weightMem, 200)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val r0 = readResult(dut, 0)
      assert(r0 == 0, s"Expected 0, got $r0")
    }
  }

  it should "F4: negative activations with mixed weights cancel" in {
    test(new BitNetAccelerator) { dut =>
      // M=1, K=4, shift=0. acts=[-1,-1,-1,-1], weights=[+1,+1,-1,-1]
      // (-1)*1 + (-1)*1 + (-1)*(-1) + (-1)*(-1) = -1-1+1+1 = 0
      val weightBase = 0x6300
      val weightMem = Map(0x6300 -> packWeights(Seq(1, 1, -1, -1)))

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      for (i <- 0 until 4) writeAct(dut, i, -1)
      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x0C, 1)  // M
      writeReg(dut, 0x10, 4)  // K
      writeReg(dut, 0x14, 0)  // shift
      writeReg(dut, 0x00, 1)  // START

      runWithMemory(dut, weightMem, 200)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val r0 = readResult(dut, 0)
      assert(r0 == 0, s"Expected 0, got $r0")
    }
  }

  // ---- G: Shift amount sweep ----

  it should "G1: raw accumulator output is consistent across shift register values" in {
    test(new BitNetAccelerator) { dut =>
      dut.clock.setTimeout(0)
      // M=1, K=16, all +1, act=4. acc=4*16=64. Raw output should be 64 regardless of shift.
      val weightBase = 0x7000
      val weightMem = (0 until 4).map(t =>
        (weightBase + t) -> packWeights(Seq(1, 1, 1, 1))
      ).toMap

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      for (i <- 0 until 16) writeAct(dut, i, 4)
      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x0C, 1)   // M
      writeReg(dut, 0x10, 16)  // K

      for (shift <- 0 until 8) {
        writeReg(dut, 0x14, shift)  // shift (ignored — raw output)
        writeReg(dut, 0x00, 1)     // START

        runWithMemory(dut, weightMem, 300)

        val status = readReg(dut, 0x04)
        assert((status & 2) != 0,
          s"Shift=$shift: Expected DONE, got status=0x${status.toString(16)}")

        val r0 = readResult(dut, 0)
        assert(r0 == 64,
          s"Shift=$shift: expected raw accumulator 64, got $r0")
      }
    }
  }

  // ---- H: End-to-end clamping tests ----

  it should "H1: raw accumulator for large negative value (no clamping)" in {
    test(new BitNetAccelerator) { dut =>
      // all -1 × act=100 → acc=-400 (raw, no clamp)
      val weightBase = 0x8000
      val weightMem = Map(0x8000 -> packWeights(Seq(-1, -1, -1, -1)))

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      for (i <- 0 until 4) writeAct(dut, i, 100)
      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x0C, 1)  // M
      writeReg(dut, 0x10, 4)  // K
      writeReg(dut, 0x14, 0)  // shift
      writeReg(dut, 0x00, 1)  // START

      runWithMemory(dut, weightMem, 200)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val r0 = readResult(dut, 0)
      assert(r0 == -400, s"Expected -400, got $r0")
    }
  }

  it should "H2: accumulator exactly 127 (no clamp needed)" in {
    test(new BitNetAccelerator) { dut =>
      // all +1 × acts=[42,42,42,1] → 127 exactly
      val weightBase = 0x8100
      val weightMem = Map(0x8100 -> packWeights(Seq(1, 1, 1, 1)))

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      val acts = Seq(42, 42, 42, 1)
      for (i <- acts.indices) writeAct(dut, i, acts(i))
      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x0C, 1)  // M
      writeReg(dut, 0x10, 4)  // K
      writeReg(dut, 0x14, 0)  // shift
      writeReg(dut, 0x00, 1)  // START

      runWithMemory(dut, weightMem, 200)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val r0 = readResult(dut, 0)
      assert(r0 == 127, s"Expected 127, got $r0")
    }
  }

  it should "H3: accumulator exactly -128 (no clamp needed)" in {
    test(new BitNetAccelerator) { dut =>
      // all -1 × acts=[32,32,32,32] → -128 exactly
      val weightBase = 0x8200
      val weightMem = Map(0x8200 -> packWeights(Seq(-1, -1, -1, -1)))

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      for (i <- 0 until 4) writeAct(dut, i, 32)
      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x0C, 1)  // M
      writeReg(dut, 0x10, 4)  // K
      writeReg(dut, 0x14, 0)  // shift
      writeReg(dut, 0x00, 1)  // START

      runWithMemory(dut, weightMem, 200)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val r0 = readResult(dut, 0)
      assert(r0 == -128, s"Expected -128, got $r0")
    }
  }

  it should "H4: raw accumulator 128 (no clamping)" in {
    test(new BitNetAccelerator) { dut =>
      // all +1 × acts=[32,32,32,32] → 128 (raw)
      val weightBase = 0x8300
      val weightMem = Map(0x8300 -> packWeights(Seq(1, 1, 1, 1)))

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      for (i <- 0 until 4) writeAct(dut, i, 32)
      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x0C, 1)  // M
      writeReg(dut, 0x10, 4)  // K
      writeReg(dut, 0x14, 0)  // shift
      writeReg(dut, 0x00, 1)  // START

      runWithMemory(dut, weightMem, 200)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val r0 = readResult(dut, 0)
      assert(r0 == 128, s"Expected 128, got $r0")
    }
  }

  it should "H5: raw accumulator -192 (no clamping)" in {
    test(new BitNetAccelerator) { dut =>
      // all -1 × acts=[48,48,48,48] → -192 (raw)
      val weightBase = 0x8400
      val weightMem = Map(0x8400 -> packWeights(Seq(-1, -1, -1, -1)))

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      for (i <- 0 until 4) writeAct(dut, i, 48)
      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x0C, 1)  // M
      writeReg(dut, 0x10, 4)  // K
      writeReg(dut, 0x14, 0)  // shift
      writeReg(dut, 0x00, 1)  // START

      runWithMemory(dut, weightMem, 200)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val r0 = readResult(dut, 0)
      assert(r0 == -192, s"Expected -192, got $r0")
    }
  }

  // ---- J: Performance counter ----

  it should "J1: performance counter reports cycles and scales with K" in {
    test(new BitNetAccelerator) { dut =>
      dut.clock.setTimeout(0)
      val weightBase1 = 0xA000
      val weightBase2 = 0xA100
      val weightMem = Map(
        0xA000 -> packWeights(Seq(1, 1, 1, 1)),
        0xA100 -> packWeights(Seq(1, 1, 1, 1)),
        0xA101 -> packWeights(Seq(1, 1, 1, 1)),
        0xA102 -> packWeights(Seq(1, 1, 1, 1)),
        0xA103 -> packWeights(Seq(1, 1, 1, 1))
      )

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      // Run 1: M=1, K=4 (1 tile)
      for (i <- 0 until 4) writeAct(dut, i, 1)
      writeReg(dut, 0x08, weightBase1)
      writeReg(dut, 0x0C, 1)  // M
      writeReg(dut, 0x10, 4)  // K
      writeReg(dut, 0x14, 0)  // shift
      writeReg(dut, 0x00, 1)  // START

      runWithMemory(dut, weightMem, 200)

      val status1 = readReg(dut, 0x04)
      assert((status1 & 2) != 0, "Run 1: Expected DONE")

      val cycles1 = readReg(dut, 0x18)
      assert(cycles1 > 0, s"Expected perf counter > 0, got $cycles1")

      // Run 2: M=1, K=16 (4 tiles, should take more cycles)
      for (i <- 0 until 16) writeAct(dut, i, 1)
      writeReg(dut, 0x08, weightBase2)
      writeReg(dut, 0x0C, 1)   // M
      writeReg(dut, 0x10, 16)  // K
      writeReg(dut, 0x14, 0)   // shift
      writeReg(dut, 0x00, 1)   // START

      runWithMemory(dut, weightMem, 300)

      val status2 = readReg(dut, 0x04)
      assert((status2 & 2) != 0, "Run 2: Expected DONE")

      val cycles2 = readReg(dut, 0x18)
      assert(cycles2 > cycles1,
        s"Expected K=16 cycles ($cycles2) > K=4 cycles ($cycles1)")
    }
  }

  // ---- K: Back-to-back computation tests ----

  it should "K1: back-to-back computations with no state leak" in {
    test(new BitNetAccelerator) { dut =>
      val weightBase1 = 0xB000
      val weightBase2 = 0xB100
      val weightMem = Map(
        0xB000 -> packWeights(Seq(1, 1, 1, 1)),
        0xB100 -> packWeights(Seq(0, 0, 0, 0))
      )

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      // Run 1: all +1, act=1 → 4
      for (i <- 0 until 4) writeAct(dut, i, 1)
      writeReg(dut, 0x08, weightBase1)
      writeReg(dut, 0x0C, 1)  // M
      writeReg(dut, 0x10, 4)  // K
      writeReg(dut, 0x14, 0)  // shift
      writeReg(dut, 0x00, 1)  // START

      runWithMemory(dut, weightMem, 200)

      val status1 = readReg(dut, 0x04)
      assert((status1 & 2) != 0, "Run 1: Expected DONE")

      val r1 = readResult(dut, 0)
      assert(r1 == 4, s"Run 1: expected 4, got $r1")

      // Run 2: all 0, act=100 → 0 (verify no residual accumulation)
      for (i <- 0 until 4) writeAct(dut, i, 100)
      writeReg(dut, 0x08, weightBase2)
      writeReg(dut, 0x0C, 1)  // M
      writeReg(dut, 0x10, 4)  // K
      writeReg(dut, 0x14, 0)  // shift
      writeReg(dut, 0x00, 1)  // START

      runWithMemory(dut, weightMem, 200)

      val status2 = readReg(dut, 0x04)
      assert((status2 & 2) != 0, "Run 2: Expected DONE")

      val r2 = readResult(dut, 0)
      assert(r2 == 0, s"Run 2: expected 0, got $r2 (state leak from run 1!)")
    }
  }

  it should "K2: back-to-back with dimension change" in {
    test(new BitNetAccelerator) { dut =>
      val weightBase1 = 0xB200
      val weightBase2 = 0xB300
      val weightMem = Map(
        // Run 1: M=1, K=4, all +1
        0xB200 -> packWeights(Seq(1, 1, 1, 1)),
        // Run 2: M=2, K=8, tilesPerRow=2
        0xB300 -> packWeights(Seq(1, 1, 1, 1)),     // row 0 tile 0
        0xB301 -> packWeights(Seq(1, 1, 1, 1)),     // row 0 tile 1
        0xB302 -> packWeights(Seq(-1, -1, -1, -1)), // row 1 tile 0
        0xB303 -> packWeights(Seq(-1, -1, -1, -1))  // row 1 tile 1
      )

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      // Run 1: M=1, K=4, act=1, shift=0 → 4
      for (i <- 0 until 4) writeAct(dut, i, 1)
      writeReg(dut, 0x08, weightBase1)
      writeReg(dut, 0x0C, 1)  // M
      writeReg(dut, 0x10, 4)  // K
      writeReg(dut, 0x14, 0)  // shift
      writeReg(dut, 0x00, 1)  // START

      runWithMemory(dut, weightMem, 200)

      val status1 = readReg(dut, 0x04)
      assert((status1 & 2) != 0, "Run 1: Expected DONE")

      val r1 = readResult(dut, 0)
      assert(r1 == 4, s"Run 1: expected 4, got $r1")

      // Run 2: M=2, K=8, shift=1, act=2
      // Row 0: +1*2*8 = 16 (raw accumulator)
      // Row 1: -1*2*8 = -16 (raw accumulator)
      for (i <- 0 until 8) writeAct(dut, i, 2)
      writeReg(dut, 0x08, weightBase2)
      writeReg(dut, 0x0C, 2)  // M
      writeReg(dut, 0x10, 8)  // K
      writeReg(dut, 0x14, 1)  // shift
      writeReg(dut, 0x00, 1)  // START

      runWithMemory(dut, weightMem, 300)

      val status2 = readReg(dut, 0x04)
      assert((status2 & 2) != 0, "Run 2: Expected DONE")

      val r2_0 = readResult(dut, 0)
      val r2_1 = readResult(dut, 1)
      assert(r2_0 == 16, s"Run 2 Row 0: expected 16, got $r2_0")
      assert(r2_1 == -16, s"Run 2 Row 1: expected -16, got $r2_1")
    }
  }

  // ---- A3: M=8 multi-row ----

  it should "A3: compute M=8 multi-row with progressive weight patterns" in {
    test(new BitNetAccelerator) { dut =>
      // M=8, K=4, shift=0, acts=[2,2,2,2]
      val weightBase = 0xC000
      val weightMem = Map(
        0xC000 -> packWeights(Seq(1, 0, 0, 0)),     // row 0 → 2
        0xC001 -> packWeights(Seq(1, 1, 0, 0)),     // row 1 → 4
        0xC002 -> packWeights(Seq(1, 1, 1, 0)),     // row 2 → 6
        0xC003 -> packWeights(Seq(1, 1, 1, 1)),     // row 3 → 8
        0xC004 -> packWeights(Seq(-1, 0, 0, 0)),    // row 4 → -2
        0xC005 -> packWeights(Seq(-1, -1, 0, 0)),   // row 5 → -4
        0xC006 -> packWeights(Seq(-1, -1, -1, 0)),  // row 6 → -6
        0xC007 -> packWeights(Seq(-1, 0, 1, 0))     // row 7 → 0
      )

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      for (i <- 0 until 4) writeAct(dut, i, 2)
      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x0C, 8)  // M
      writeReg(dut, 0x10, 4)  // K
      writeReg(dut, 0x14, 0)  // shift
      writeReg(dut, 0x00, 1)  // START

      runWithMemory(dut, weightMem, 500)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val expected = Seq(2, 4, 6, 8, -2, -4, -6, 0)
      for (row <- 0 until 8) {
        val result = readResult(dut, row)
        assert(result == expected(row),
          s"Row $row: expected ${expected(row)}, got $result")
      }
    }
  }

  // ---- B4: Tile cancellation ----

  it should "B4: multi-tile cancellation (tile0 all +1, tile1 all -1)" in {
    test(new BitNetAccelerator) { dut =>
      // M=1, K=8, shift=0. tile0: 5*4=20, tile1: -5*4=-20, total=0
      val weightBase = 0xC100
      val weightMem = Map(
        0xC100 -> packWeights(Seq(1, 1, 1, 1)),    // tile 0
        0xC101 -> packWeights(Seq(-1, -1, -1, -1)) // tile 1
      )

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      for (i <- 0 until 8) writeAct(dut, i, 5)
      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x0C, 1)  // M
      writeReg(dut, 0x10, 8)  // K
      writeReg(dut, 0x14, 0)  // shift
      writeReg(dut, 0x00, 1)  // START

      runWithMemory(dut, weightMem, 300)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val r0 = readResult(dut, 0)
      assert(r0 == 0, s"Expected 0, got $r0")
    }
  }

  // ---- B5: Partial tile ----

  it should "B5: partial tile with zero-padded weights" in {
    test(new BitNetAccelerator) { dut =>
      // M=1, K=8, shift=0. tile0: [+1,+1,+1,+1]*1=4, tile1: [+1,+1,0,0]*1=2, total=6
      val weightBase = 0xC200
      val weightMem = Map(
        0xC200 -> packWeights(Seq(1, 1, 1, 1)),  // tile 0: all +1
        0xC201 -> packWeights(Seq(1, 1, 0, 0))   // tile 1: partial
      )

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      for (i <- 0 until 8) writeAct(dut, i, 1)
      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x0C, 1)  // M
      writeReg(dut, 0x10, 8)  // K
      writeReg(dut, 0x14, 0)  // shift
      writeReg(dut, 0x00, 1)  // START

      runWithMemory(dut, weightMem, 300)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val r0 = readResult(dut, 0)
      assert(r0 == 6, s"Expected 6, got $r0")
    }
  }

  // ---- C1: Multi-row + multi-tile mixed ----

  it should "C1: multi-row multi-tile with mixed patterns within a row" in {
    test(new BitNetAccelerator) { dut =>
      // M=2, K=8, shift=1
      // Row 0: both tiles +1 → 1*8=8 (raw accumulator)
      // Row 1: tile0 +1, tile1 -1 → 4+(-4)=0 (raw accumulator)
      val weightBase = 0xC300
      val weightMem = Map(
        0xC300 -> packWeights(Seq(1, 1, 1, 1)),    // row 0 tile 0
        0xC301 -> packWeights(Seq(1, 1, 1, 1)),    // row 0 tile 1
        0xC302 -> packWeights(Seq(1, 1, 1, 1)),    // row 1 tile 0
        0xC303 -> packWeights(Seq(-1, -1, -1, -1)) // row 1 tile 1
      )

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      for (i <- 0 until 8) writeAct(dut, i, 1)
      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x0C, 2)  // M
      writeReg(dut, 0x10, 8)  // K
      writeReg(dut, 0x14, 1)  // shift
      writeReg(dut, 0x00, 1)  // START

      runWithMemory(dut, weightMem, 300)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val r0 = readResult(dut, 0)
      val r1 = readResult(dut, 1)
      assert(r0 == 8, s"Row 0: expected 8, got $r0")
      assert(r1 == 0, s"Row 1: expected 0, got $r1")
    }
  }

  // ---- D3: M=16 max boundary ----

  it should "D3: M=16 max boundary with distinct weight patterns" in {
    test(new BitNetAccelerator) { dut =>
      // M=16, K=4, shift=0, acts=[1,1,1,1]
      val weightBase = 0xD000
      val patterns = Seq(
        Seq(1, 0, 0, 0),     // row 0  → 1
        Seq(0, 1, 0, 0),     // row 1  → 1
        Seq(0, 0, 1, 0),     // row 2  → 1
        Seq(0, 0, 0, 1),     // row 3  → 1
        Seq(1, 1, 0, 0),     // row 4  → 2
        Seq(0, 0, 1, 1),     // row 5  → 2
        Seq(1, 1, 1, 0),     // row 6  → 3
        Seq(0, 1, 1, 1),     // row 7  → 3
        Seq(1, 1, 1, 1),     // row 8  → 4
        Seq(-1, 0, 0, 0),    // row 9  → -1
        Seq(-1, -1, 0, 0),   // row 10 → -2
        Seq(-1, -1, -1, 0),  // row 11 → -3
        Seq(-1, -1, -1, -1), // row 12 → -4
        Seq(1, -1, 1, -1),   // row 13 → 0
        Seq(1, 1, -1, -1),   // row 14 → 0
        Seq(0, 0, 0, 0)      // row 15 → 0
      )
      val weightMem = patterns.zipWithIndex.map { case (p, i) =>
        (weightBase + i) -> packWeights(p)
      }.toMap

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      for (i <- 0 until 4) writeAct(dut, i, 1)
      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x0C, 16)  // M
      writeReg(dut, 0x10, 4)   // K
      writeReg(dut, 0x14, 0)   // shift
      writeReg(dut, 0x00, 1)   // START

      runWithMemory(dut, weightMem, 800)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val expected = Seq(1, 1, 1, 1, 2, 2, 3, 3, 4, -1, -2, -3, -4, 0, 0, 0)
      for (row <- 0 until 16) {
        val result = readResult(dut, row)
        assert(result == expected(row),
          s"Row $row: expected ${expected(row)}, got $result")
      }
    }
  }

  // ---- E1: All three weight types in single computation ----

  it should "E1: all three weight types across tiles" in {
    test(new BitNetAccelerator) { dut =>
      // M=1, K=12, shift=0. tile0: all +1, tile1: all 0, tile2: all -1
      // acts=[3,3,3,3, 10,10,10,10, 1,1,1,1] → 12+0-4=8
      val weightBase = 0xD100
      val weightMem = Map(
        0xD100 -> packWeights(Seq(1, 1, 1, 1)),    // tile 0: all +1
        0xD101 -> packWeights(Seq(0, 0, 0, 0)),    // tile 1: all 0
        0xD102 -> packWeights(Seq(-1, -1, -1, -1)) // tile 2: all -1
      )

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      val acts = Seq(3, 3, 3, 3, 10, 10, 10, 10, 1, 1, 1, 1)
      for (i <- acts.indices) writeAct(dut, i, acts(i))
      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x0C, 1)   // M
      writeReg(dut, 0x10, 12)  // K
      writeReg(dut, 0x14, 0)   // shift
      writeReg(dut, 0x00, 1)   // START

      runWithMemory(dut, weightMem, 300)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val r0 = readResult(dut, 0)
      assert(r0 == 8, s"Expected 8, got $r0")
    }
  }

  // ---- L2: Known-answer vector with multi-row + clamp ----

  it should "L2: known-answer vector with cancellation and clamping" in {
    test(new BitNetAccelerator) { dut =>
      // M=2, K=8, shift=0, acts all=100
      // Row 0: tile0 +1, tile1 -1 → 400+(-400) = 0
      // Row 1: all +1 → 800 (raw accumulator)
      val weightBase = 0xD200
      val weightMem = Map(
        0xD200 -> packWeights(Seq(1, 1, 1, 1)),     // row 0 tile 0
        0xD201 -> packWeights(Seq(-1, -1, -1, -1)),  // row 0 tile 1
        0xD202 -> packWeights(Seq(1, 1, 1, 1)),     // row 1 tile 0
        0xD203 -> packWeights(Seq(1, 1, 1, 1))      // row 1 tile 1
      )

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      for (i <- 0 until 8) writeAct(dut, i, 100)
      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x0C, 2)  // M
      writeReg(dut, 0x10, 8)  // K
      writeReg(dut, 0x14, 0)  // shift
      writeReg(dut, 0x00, 1)  // START

      runWithMemory(dut, weightMem, 300)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val r0 = readResult(dut, 0)
      val r1 = readResult(dut, 1)
      assert(r0 == 0, s"Row 0: expected 0, got $r0")
      assert(r1 == 800, s"Row 1: expected 800 (raw accumulator), got $r1")
    }
  }

  // ---- M: K=2048 tests (maxDimK extension) ----

  it should "M1: compute M=1 with K=2048 (512 tiles, max-K deep accumulation)" in {
    val largeCfg = BitNetConfig(
      numPEs = 4,
      maxDimM = 16,
      maxDimK = 2048,
      avalonDataW = 8
    )
    test(new BitNetAccelerator()(largeCfg)) { dut =>
      dut.clock.setTimeout(0)
      val K = 2048
      val shift = 5
      val weightBase = 0xE000
      val tilesPerRow = K / 4 // 512
      // Every 4th weight = +1, rest 0. Per tile: [+1,0,0,0]·[4,4,4,4] = 4.
      // 512 tiles × 4 = 2048 (raw accumulator).
      val weightPacked = packWeights(Seq(1, 0, 0, 0))
      val weightMem = (0 until tilesPerRow).map(t => (weightBase + t) -> weightPacked).toMap

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      for (i <- 0 until K) writeReg(dut, 0x80 + i * 4, 4)
      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x0C, 1)     // M
      writeReg(dut, 0x10, K)     // K
      writeReg(dut, 0x14, shift) // shift
      writeReg(dut, 0x00, 1)     // START

      runWithMemory(dut, weightMem, 10000)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val result = readResult(dut, 0)
      assert(result == 2048, s"Expected 2048, got $result")
    }
  }

  it should "M2: compute M=4 multi-row with K=2048" in {
    val largeCfg = BitNetConfig(
      numPEs = 4,
      maxDimM = 16,
      maxDimK = 2048,
      avalonDataW = 8
    )
    test(new BitNetAccelerator()(largeCfg)) { dut =>
      dut.clock.setTimeout(0)
      val K = 2048
      val shift = 5
      val weightBase = 0xF000
      val tilesPerRow = K / 4 // 512

      // Row 0: all +1  → 2048*1=2048 (raw accumulator)
      // Row 1: all -1  → -2048 (raw accumulator)
      // Row 2: all  0  → 0
      // Row 3: alt +1,-1,+1,-1 → 0 per tile → 0
      val rowWeights = Seq(
        packWeights(Seq(1, 1, 1, 1)),
        packWeights(Seq(-1, -1, -1, -1)),
        packWeights(Seq(0, 0, 0, 0)),
        packWeights(Seq(1, -1, 1, -1))
      )
      val weightMem = (0 until 4).flatMap { row =>
        (0 until tilesPerRow).map { t =>
          (weightBase + row * tilesPerRow + t) -> rowWeights(row)
        }
      }.toMap

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      for (i <- 0 until K) writeReg(dut, 0x80 + i * 4, 1)
      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x0C, 4)     // M
      writeReg(dut, 0x10, K)     // K
      writeReg(dut, 0x14, shift) // shift
      writeReg(dut, 0x00, 1)     // START

      runWithMemory(dut, weightMem, 40000)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val expected = Seq(2048, -2048, 0, 0)
      for (row <- 0 until 4) {
        val result = readResult(dut, row)
        assert(result == expected(row),
          s"Row $row: expected ${expected(row)}, got $result")
      }
    }
  }

  it should "M3: back-to-back K=1024 then K=2048 dimension change" in {
    val largeCfg = BitNetConfig(
      numPEs = 4,
      maxDimM = 16,
      maxDimK = 2048,
      avalonDataW = 8
    )
    test(new BitNetAccelerator()(largeCfg)) { dut =>
      dut.clock.setTimeout(0)

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      // Run 1: K=1024, M=1, shift=4. All +1, act=4.
      // 256 tiles × 4 = 1024 (raw accumulator).
      val K1 = 1024
      val tilesPerRow1 = K1 / 4
      val weightBase1 = 0x10000
      val weightPacked1 = packWeights(Seq(1, 0, 0, 0))
      val weightMem1 = (0 until tilesPerRow1).map(t =>
        (weightBase1 + t) -> weightPacked1
      ).toMap

      for (i <- 0 until K1) writeReg(dut, 0x80 + i * 4, 4)
      writeReg(dut, 0x08, weightBase1)
      writeReg(dut, 0x0C, 1)   // M
      writeReg(dut, 0x10, K1)  // K
      writeReg(dut, 0x14, 4)   // shift
      writeReg(dut, 0x00, 1)   // START

      runWithMemory(dut, weightMem1, 5000)

      val status1 = readReg(dut, 0x04)
      assert((status1 & 2) != 0, "Run 1: Expected DONE")
      val r1 = readResult(dut, 0)
      assert(r1 == 1024, s"Run 1: expected 1024, got $r1")

      // Run 2: K=2048, M=1, shift=5. All +1, act=4.
      // 512 tiles × 4 = 2048 (raw accumulator).
      val K2 = 2048
      val tilesPerRow2 = K2 / 4
      val weightBase2 = 0x20000
      val weightPacked2 = packWeights(Seq(1, 0, 0, 0))
      val weightMem2 = (0 until tilesPerRow2).map(t =>
        (weightBase2 + t) -> weightPacked2
      ).toMap

      for (i <- 0 until K2) writeReg(dut, 0x80 + i * 4, 4)
      writeReg(dut, 0x08, weightBase2)
      writeReg(dut, 0x0C, 1)   // M
      writeReg(dut, 0x10, K2)  // K
      writeReg(dut, 0x14, 5)   // shift
      writeReg(dut, 0x00, 1)   // START

      runWithMemory(dut, weightMem2, 10000)

      val status2 = readReg(dut, 0x04)
      assert((status2 & 2) != 0, "Run 2: Expected DONE")
      val r2 = readResult(dut, 0)
      assert(r2 == 2048, s"Run 2: expected 2048, got $r2")
    }
  }

  // ---- N: DDR3 mode tests ----
  // Use a 32-bit bus config for DDR3 tests so INT32 results fit in one beat
  val ddr3Cfg: BitNetConfig = BitNetConfig(
    numPEs = 4,
    maxDimM = 16,
    maxDimK = 16,
    avalonDataW = 32   // 32-bit bus: 4 INT8 acts per beat, 1 INT32 result per beat
  )

  /** Pack INT8 activations into DDR3 memory entries for a given config. */
  def packActsDdr3(acts: Seq[Int], c: BitNetConfig): Seq[(Int, BigInt)] = {
    val actsPerBeat = (c.avalonDataW / c.activationW).max(1)
    val bytesPerBeat = c.avalonDataW / 8
    acts.grouped(actsPerBeat).zipWithIndex.map { case (group, idx) =>
      var packed = BigInt(0)
      for (i <- group.indices) {
        packed = packed | (BigInt(group(i) & 0xFF) << (i * c.activationW))
      }
      (idx * bytesPerBeat, packed)
    }.toSeq
  }

  /** Unpack INT32 results from DDR3 memory map written by ResultWriter. */
  def unpackResults(ddr3Mem: scala.collection.mutable.Map[Int, BigInt],
                    baseAddr: Int, count: Int, c: BitNetConfig): Seq[Int] = {
    val resultsPerBeat = (c.avalonDataW / 32).max(1)
    val bytesPerBeat = c.avalonDataW / 8
    (0 until count).map { i =>
      val beatIdx = i / resultsPerBeat
      val slotIdx = i % resultsPerBeat
      val addr = baseAddr + beatIdx * bytesPerBeat
      val beatData = ddr3Mem.getOrElse(addr, BigInt(0))
      val raw32 = (beatData >> (slotIdx * 32)) & BigInt(0xFFFFFFFFL)
      if (raw32 > 0x7FFFFFFFL) (raw32 - BigInt(0x100000000L)).toInt else raw32.toInt
    }
  }

  /** Pack weights for a given config (4 PEs, 2 bits each). */
  def packWeightsDdr3(weights: Seq[Int]): BigInt = {
    var packed = BigInt(0)
    for (i <- weights.indices) {
      val enc = weights(i) match {
        case 0  => 0
        case 1  => 1
        case -1 => 2
      }
      packed = packed | (BigInt(enc) << (i * 2))
    }
    packed
  }

  /** Run DDR3 mode memory model for a given config. Handles reads and writes. */
  def runDdr3Memory(dut: BitNetAccelerator, ddr3Mem: scala.collection.mutable.Map[Int, BigInt],
                    cycles: Int, c: BitNetConfig): Unit = {
    val bytesPerBeat = c.avalonDataW / 8
    var burstAddr = 0
    var burstRemaining = 0
    var newBurstPending = false
    var newBurstAddr = 0
    var newBurstLen = 0

    for (_ <- 0 until cycles) {
      if (newBurstPending) {
        burstAddr = newBurstAddr
        burstRemaining = newBurstLen
        newBurstPending = false
      }

      if (burstRemaining > 0) {
        dut.io.master.readdatavalid.poke(true.B)
        val data = ddr3Mem.getOrElse(burstAddr, BigInt(0))
        dut.io.master.readdata.poke(data.U)
        burstAddr += bytesPerBeat
        burstRemaining -= 1
      } else {
        dut.io.master.readdatavalid.poke(false.B)
      }

      if (dut.io.master.read.peek().litToBoolean) {
        newBurstAddr = dut.io.master.address.peek().litValue.toInt
        newBurstLen = dut.io.master.burstcount.peek().litValue.toInt
        newBurstPending = true
      }

      if (dut.io.master.write.peek().litToBoolean) {
        val writeAddr = dut.io.master.address.peek().litValue.toInt
        val writeData = dut.io.master.writedata.peek().litValue
        ddr3Mem(writeAddr) = writeData
      }

      dut.clock.step(1)
    }
  }

  it should "N1: DDR3 mode end-to-end M=2 K=4" in {
    val c = ddr3Cfg
    test(new BitNetAccelerator()(c)) { dut =>
      val weightBase = 0x1000
      val actBase = 0x2000
      val resBase = 0x3000

      val weightMem = Map(
        0x1000 -> packWeightsDdr3(Seq(1, 1, 1, 1)),
        0x1004 -> packWeightsDdr3(Seq(-1, -1, -1, -1))
      )

      val ddr3Mem = scala.collection.mutable.Map[Int, BigInt]()
      weightMem.foreach { case (k, v) => ddr3Mem(k) = v }
      for ((offset, data) <- packActsDdr3(Seq(1, 1, 1, 1), c)) {
        ddr3Mem(actBase + offset) = data
      }

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x28, actBase)
      writeReg(dut, 0x2C, resBase)
      writeReg(dut, 0x0C, 2)   // M
      writeReg(dut, 0x10, 4)   // K
      writeReg(dut, 0x14, 0)   // shift
      writeReg(dut, 0x00, 3)   // START + DDR3_MODE

      runDdr3Memory(dut, ddr3Mem, 300, c)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val results = unpackResults(ddr3Mem, resBase, 2, c)
      assert(results(0) == 4, s"Row 0: expected 4, got ${results(0)}")
      assert(results(1) == -4, s"Row 1: expected -4, got ${results(1)}")
    }
  }

  it should "N2: DDR3 mode M=4 with varying weight patterns" in {
    val c = ddr3Cfg
    test(new BitNetAccelerator()(c)) { dut =>
      val weightBase = 0x4000
      val actBase = 0x5000
      val resBase = 0x6000

      val weightMem = Map(
        0x4000 -> packWeightsDdr3(Seq(1, 1, 1, 1)),
        0x4004 -> packWeightsDdr3(Seq(0, 0, 0, 0)),
        0x4008 -> packWeightsDdr3(Seq(-1, -1, -1, -1)),
        0x400C -> packWeightsDdr3(Seq(1, -1, 1, -1))
      )

      val ddr3Mem = scala.collection.mutable.Map[Int, BigInt]()
      weightMem.foreach { case (k, v) => ddr3Mem(k) = v }
      for ((offset, data) <- packActsDdr3(Seq(1, 2, 3, 4), c)) {
        ddr3Mem(actBase + offset) = data
      }

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x28, actBase)
      writeReg(dut, 0x2C, resBase)
      writeReg(dut, 0x0C, 4)   // M
      writeReg(dut, 0x10, 4)   // K
      writeReg(dut, 0x14, 0)   // shift
      writeReg(dut, 0x00, 3)   // START + DDR3_MODE

      runDdr3Memory(dut, ddr3Mem, 500, c)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val results = unpackResults(ddr3Mem, resBase, 4, c)
      val expected = Seq(10, 0, -10, -2)
      for (row <- 0 until 4) {
        assert(results(row) == expected(row),
          s"Row $row: expected ${expected(row)}, got ${results(row)}")
      }
    }
  }

  it should "N3: DDR3 mode with negative activations" in {
    val c = ddr3Cfg
    test(new BitNetAccelerator()(c)) { dut =>
      val weightBase = 0x7000
      val actBase = 0x7100
      val resBase = 0x7200

      val weightMem = Map(0x7000 -> packWeightsDdr3(Seq(1, 1, 1, 1)))

      val ddr3Mem = scala.collection.mutable.Map[Int, BigInt]()
      weightMem.foreach { case (k, v) => ddr3Mem(k) = v }
      for ((offset, data) <- packActsDdr3(Seq(-2, -2, -2, -2), c)) {
        ddr3Mem(actBase + offset) = data
      }

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x28, actBase)
      writeReg(dut, 0x2C, resBase)
      writeReg(dut, 0x0C, 1)   // M
      writeReg(dut, 0x10, 4)   // K
      writeReg(dut, 0x14, 0)   // shift
      writeReg(dut, 0x00, 3)   // START + DDR3_MODE

      runDdr3Memory(dut, ddr3Mem, 300, c)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val results = unpackResults(ddr3Mem, resBase, 1, c)
      assert(results(0) == -8, s"Expected -8, got ${results(0)}")
    }
  }

  it should "N4: DDR3 mode=0 still uses legacy register path" in {
    test(new BitNetAccelerator) { dut =>
      val weightBase = 0x9000
      val weightMem = Map(0x9000 -> packWeights(Seq(1, 1, 1, 1)))

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      for (i <- 0 until 4) writeReg(dut, 0x80 + i * 4, 5)
      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x0C, 1)  // M
      writeReg(dut, 0x10, 4)  // K
      writeReg(dut, 0x14, 0)  // shift
      writeReg(dut, 0x00, 1)  // START (DDR3_MODE=0)

      runWithMemory(dut, weightMem, 200)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val r0 = readResult(dut, 0)
      assert(r0 == 20, s"Expected 20, got $r0")
    }
  }

  it should "N5: DDR3 mode with K=8 multi-tile" in {
    val c = ddr3Cfg
    test(new BitNetAccelerator()(c)) { dut =>
      val weightBase = 0xA000
      val actBase = 0xA100
      val resBase = 0xA200

      val weightMem = Map(
        0xA000 -> packWeightsDdr3(Seq(1, 1, 1, 1)),     // row 0 tile 0
        0xA004 -> packWeightsDdr3(Seq(1, 1, 1, 1)),     // row 0 tile 1
        0xA008 -> packWeightsDdr3(Seq(-1, -1, -1, -1)),  // row 1 tile 0
        0xA00C -> packWeightsDdr3(Seq(-1, -1, -1, -1))   // row 1 tile 1
      )

      val ddr3Mem = scala.collection.mutable.Map[Int, BigInt]()
      weightMem.foreach { case (k, v) => ddr3Mem(k) = v }
      for ((offset, data) <- packActsDdr3(Seq(1, 1, 1, 1, 1, 1, 1, 1), c)) {
        ddr3Mem(actBase + offset) = data
      }

      dut.io.master.waitrequest.poke(false.B)
      dut.io.master.readdatavalid.poke(false.B)
      dut.io.master.readdata.poke(0.U)

      writeReg(dut, 0x08, weightBase)
      writeReg(dut, 0x28, actBase)
      writeReg(dut, 0x2C, resBase)
      writeReg(dut, 0x0C, 2)   // M
      writeReg(dut, 0x10, 8)   // K
      writeReg(dut, 0x14, 0)   // shift
      writeReg(dut, 0x00, 3)   // START + DDR3_MODE

      runDdr3Memory(dut, ddr3Mem, 400, c)

      val status = readReg(dut, 0x04)
      assert((status & 2) != 0, s"Expected DONE, got status=0x${status.toString(16)}")

      val results = unpackResults(ddr3Mem, resBase, 2, c)
      assert(results(0) == 8, s"Row 0: expected 8, got ${results(0)}")
      assert(results(1) == -8, s"Row 1: expected -8, got ${results(1)}")
    }
  }
}
