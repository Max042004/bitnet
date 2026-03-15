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

/** Streams an entire row of weight tiles from DDR3 via pipelined Avalon burst reads.
  *
  * Pipelined sub-bursts: instead of issuing one large burst per row, issues
  * multiple sub-bursts of maxBurstLen beats back-to-back. The next sub-burst
  * is issued as soon as the previous one's read command is accepted (not waiting
  * for data), keeping multiple reads in the DDR3 pipeline simultaneously.
  *
  * Double-buffered: two FIFOs (A and B) allow prefetching the next row while
  * the current row is being consumed. The `swap` signal toggles which FIFO
  * is the active (consumer) side vs. the fill side.
  *
  * When avalonDataW < weightDataW (e.g. 128-bit bus, 256-bit tiles), multiple
  * bus beats are assembled into one FIFO entry before enqueueing.
  *
  * Address = baseAddr + rowIdx * tilesPerRow * (weightDataW / 8)
  */
class WeightStreamer(implicit val cfg: BitNetConfig) extends Module {
  val io = IO(new Bundle {
    // Control
    val startRow = Input(Bool())
    val baseAddr = Input(UInt(cfg.avalonAddrW.W))
    val dimK     = Input(UInt(cfg.dimW.W))
    val rowIdx   = Input(UInt(cfg.dimW.W))

    // Double-buffer control
    val swap         = Input(Bool())
    val prefetchDone = Output(Bool())

    // Avalon-MM master (bus width)
    val avalon = new AvalonMMReadMaster(cfg.avalonAddrW, cfg.avalonDataW, cfg.burstCountW)

    // FIFO dequeue interface (internal weight data width)
    val weightData = Output(UInt(cfg.weightDataW.W))
    val dataReady  = Output(Bool())
    val dequeue    = Input(Bool())

    // Status
    val rowDone = Output(Bool())
  })

  val sIdle :: sBurst :: Nil = Enum(2)
  val state = RegInit(sIdle)

  val busBytesPerBeat = (cfg.avalonDataW / 8)
  // Physical tile size in DDR3: beatsPerTile bus beats per tile
  val tileBytesSize   = (cfg.beatsPerTile * busBytesPerBeat)
  val addr = RegInit(0.U(cfg.avalonAddrW.W))

  // Pipelined burst tracking
  val totalBusBeats  = RegInit(0.U(cfg.dimW.W))  // total bus beats for this row
  val beatsIssued    = RegInit(0.U(cfg.dimW.W))   // bus beats with read command accepted
  val beatsReceived  = RegInit(0.U(cfg.dimW.W))   // bus beats with data received

  // Track tiles per row for swap/rowDone
  val tilesPerRow = RegInit(0.U(cfg.dimW.W))
  val activeTilesPerRow = RegInit(0.U(cfg.dimW.W))
  val deqCount = RegInit(0.U(cfg.dimW.W))

  // Double-buffered FIFOs (internal weight data width)
  val maxTilesPerRow = cfg.maxDimK / cfg.numPEs
  val fifoA = Module(new Queue(UInt(cfg.weightDataW.W), maxTilesPerRow))
  val fifoB = Module(new Queue(UInt(cfg.weightDataW.W), maxTilesPerRow))

  // activeBuf: false = A is active (consumer), B is fill
  //            true  = B is active (consumer), A is fill
  val activeBuf = RegInit(false.B)

  // Fill-side FIFO is done when all beats received
  val fillDoneReg = RegInit(false.B)
  io.prefetchDone := fillDoneReg

  // Swap logic: toggle activeBuf, reset deqCount, capture tilesPerRow
  when(io.swap) {
    activeBuf := !activeBuf
    deqCount := 0.U
    activeTilesPerRow := tilesPerRow
    fillDoneReg := false.B
  }

  // --- Beat assembly: collect beatsPerTile bus beats into one weightDataW-bit entry ---
  val fillEnqValid = Wire(Bool())
  val fillEnqBits  = Wire(UInt(cfg.weightDataW.W))

  if (cfg.beatsPerTile == 1) {
    // No assembly needed: each bus beat carries a full tile
    fillEnqValid := io.avalon.readdatavalid
    fillEnqBits  := io.avalon.readdata(cfg.weightDataW - 1, 0)
  } else {
    // Assemble beatsPerTile bus beats into one FIFO entry
    val assemblyReg = Reg(UInt(cfg.avalonDataW.W))
    val subBeat = RegInit(0.U(log2Ceil(cfg.beatsPerTile).W))
    fillEnqValid := false.B
    fillEnqBits  := Cat(io.avalon.readdata, assemblyReg)  // {upper, lower}

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

  // Enqueue to fill-side FIFO
  fifoA.io.enq.valid := Mux(activeBuf, fillEnqValid, false.B)
  fifoA.io.enq.bits  := fillEnqBits
  fifoB.io.enq.valid := Mux(!activeBuf, fillEnqValid, false.B)
  fifoB.io.enq.bits  := fillEnqBits

  // Dequeue from active-side FIFO
  val activeDeqValid = Mux(activeBuf, fifoB.io.deq.valid, fifoA.io.deq.valid)
  val activeDeqBits  = Mux(activeBuf, fifoB.io.deq.bits, fifoA.io.deq.bits)

  io.weightData := activeDeqBits
  io.dataReady  := activeDeqValid

  fifoA.io.deq.ready := Mux(!activeBuf, io.dequeue, false.B)
  fifoB.io.deq.ready := Mux(activeBuf, io.dequeue, false.B)

  // Track dequeues for rowDone on active side
  when(io.swap) {
    deqCount := 0.U
  }.elsewhen((fifoA.io.deq.fire && !activeBuf) || (fifoB.io.deq.fire && activeBuf)) {
    deqCount := deqCount + 1.U
  }

  io.rowDone := deqCount === activeTilesPerRow && activeTilesPerRow =/= 0.U

  // Clear fillDone when starting a new fill
  when(io.startRow && state === sIdle) {
    fillDoneReg := false.B
  }

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
      when(io.startRow) {
        val tpr = (io.dimK + (cfg.numPEs - 1).U) >> log2Ceil(cfg.numPEs).U
        tilesPerRow := tpr
        val busBeats = tpr * cfg.beatsPerTile.U
        totalBusBeats := busBeats
        beatsIssued := 0.U
        beatsReceived := 0.U
        // Row byte offset = rowIdx * tilesPerRow * tileBytesSize
        addr := io.baseAddr + io.rowIdx * tpr * tileBytesSize.U
        state := sBurst
      }
    }
    is(sBurst) {
      // Issue sub-bursts back-to-back: next burst as soon as previous accepted
      when(beatsIssued < totalBusBeats) {
        io.avalon.read := true.B
        io.avalon.address := addr
        io.avalon.burstcount := thisBurstLen(cfg.burstCountW - 1, 0)
        when(!io.avalon.waitrequest) {
          beatsIssued := beatsIssued + thisBurstLen
          addr := addr + thisBurstLen * busBytesPerBeat.U
        }
      }

      // Receive data concurrently (independent of issuing)
      when(io.avalon.readdatavalid) {
        beatsReceived := beatsReceived + 1.U
      }

      // Done when all data received
      when(beatsReceived + io.avalon.readdatavalid.asUInt === totalBusBeats) {
        fillDoneReg := true.B
        state := sIdle
      }
    }
  }
}
