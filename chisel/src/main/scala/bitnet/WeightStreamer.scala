package bitnet

import chisel3._
import chisel3.util._

/** Avalon-MM read-master interface bundle with burst support. */
class AvalonMMReadMaster(addrW: Int, dataW: Int, burstCountW: Int) extends Bundle {
  val address       = Output(UInt(addrW.W))
  val read          = Output(Bool())
  val readdata      = Input(UInt(dataW.W))
  val waitrequest   = Input(Bool())
  val readdatavalid = Input(Bool())
  val burstcount    = Output(UInt(burstCountW.W))
}

/** Streams weight tiles from DDR3 via pipelined Avalon burst reads.
  *
  * T-MAC tile-major mode: for each tile position k, streams M weight tiles
  * (one per output row) sequentially. Uses a single shallow FIFO with
  * continuous burst reads to keep the DDR3 pipeline saturated.
  *
  * Weight layout in DDR3 (tile-major):
  *   tile0: [row0][row1]...[rowM-1]
  *   tile1: [row0][row1]...[rowM-1]
  *   ...
  *
  * Address = baseAddr + tileIdx * tileStride (sequential within burst)
  *
  * When avalonDataW < weightDataW, multiple bus beats are assembled into one
  * FIFO entry before enqueueing.
  */
class WeightStreamer(implicit val cfg: BitNetConfig) extends Module {
  val io = IO(new Bundle {
    // Control: start streaming M tiles for a given tile position
    val startTile  = Input(Bool())
    val baseAddr   = Input(UInt(cfg.avalonAddrW.W))
    val dimM       = Input(UInt(cfg.dimW.W))
    val tileIdx    = Input(UInt(cfg.dimW.W))
    val tileStride = Input(UInt(cfg.avalonAddrW.W))

    // Avalon-MM master (bus width)
    val avalon = new AvalonMMReadMaster(cfg.avalonAddrW, cfg.avalonDataW, cfg.burstCountW)

    // FIFO dequeue interface (internal weight data width)
    val weightData = Output(UInt(cfg.weightDataW.W))
    val dataReady  = Output(Bool())
    val dequeue    = Input(Bool())

    // Status
    val streamDone = Output(Bool())
  })

  val sIdle :: sBurst :: Nil = Enum(2)
  val state = RegInit(sIdle)

  val busBytesPerBeat = cfg.avalonDataW / 8
  val tileBytesSize   = cfg.beatsPerTile * busBytesPerBeat

  val addr = RegInit(0.U(cfg.avalonAddrW.W))

  // Pipelined burst tracking
  val totalBusBeats  = RegInit(0.U(32.W))
  val beatsIssued    = RegInit(0.U(32.W))
  val beatsReceived  = RegInit(0.U(32.W))

  // Single shallow FIFO (enough depth to absorb DDR3 latency)
  val fifoDepth = 64
  val fifo = Module(new Queue(UInt(cfg.weightDataW.W), fifoDepth))

  val doneReg = RegInit(false.B)
  io.streamDone := doneReg

  // --- Beat assembly: collect beatsPerTile bus beats into one weightDataW-bit entry ---
  val fillEnqValid = Wire(Bool())
  val fillEnqBits  = Wire(UInt(cfg.weightDataW.W))

  if (cfg.beatsPerTile == 1) {
    fillEnqValid := io.avalon.readdatavalid
    fillEnqBits  := io.avalon.readdata(cfg.weightDataW - 1, 0)
  } else {
    val assemblyReg = Reg(UInt(cfg.avalonDataW.W))
    val subBeat = RegInit(0.U(log2Ceil(cfg.beatsPerTile).W))
    fillEnqValid := false.B
    fillEnqBits  := Cat(io.avalon.readdata, assemblyReg)

    when(io.avalon.readdatavalid) {
      when(subBeat === (cfg.beatsPerTile - 1).U) {
        fillEnqValid := true.B
        subBeat := 0.U
      }.otherwise {
        assemblyReg := io.avalon.readdata
        subBeat := subBeat + 1.U
      }
    }
  }

  // Enqueue to FIFO
  fifo.io.enq.valid := fillEnqValid
  fifo.io.enq.bits  := fillEnqBits

  // Dequeue interface
  io.weightData := fifo.io.deq.bits
  io.dataReady  := fifo.io.deq.valid
  fifo.io.deq.ready := io.dequeue

  // --- Pipelined sub-burst logic ---
  val maxBurst = cfg.maxBurstLen
  val remainingBeats = totalBusBeats - beatsIssued
  val thisBurstLen = Mux(remainingBeats > maxBurst.U, maxBurst.U, remainingBeats)

  // Avalon defaults
  io.avalon.address    := addr
  io.avalon.read       := false.B
  io.avalon.burstcount := thisBurstLen(cfg.burstCountW - 1, 0)

  switch(state) {
    is(sIdle) {
      when(io.startTile) {
        // Compute: total bus beats = dimM * beatsPerTile
        val busBeats = io.dimM * cfg.beatsPerTile.U
        totalBusBeats := busBeats
        beatsIssued := 0.U
        beatsReceived := 0.U
        // Address = baseAddr + tileIdx * tileStride
        addr := io.baseAddr + io.tileIdx * io.tileStride
        doneReg := false.B
        state := sBurst
      }
    }
    is(sBurst) {
      // Issue sub-bursts back-to-back
      when(beatsIssued < totalBusBeats) {
        io.avalon.read := true.B
        io.avalon.address := addr
        io.avalon.burstcount := thisBurstLen(cfg.burstCountW - 1, 0)
        when(!io.avalon.waitrequest) {
          beatsIssued := beatsIssued + thisBurstLen
          addr := addr + thisBurstLen * busBytesPerBeat.U
        }
      }

      // Receive data concurrently
      when(io.avalon.readdatavalid) {
        beatsReceived := beatsReceived + 1.U
      }

      // Done when all data received
      when(beatsReceived + io.avalon.readdatavalid.asUInt === totalBusBeats) {
        doneReg := true.B
        state := sIdle
      }
    }
  }
}
