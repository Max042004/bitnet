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

/** Streams an entire row of weight tiles from DDR3 via a single Avalon burst read.
  *
  * Double-buffered: two FIFOs (A and B) allow prefetching the next row while
  * the current row is being consumed. The `swap` signal toggles which FIFO
  * is the active (consumer) side vs. the fill side.
  *
  * On a `startRow` pulse, computes the row address and issues one burst read
  * of `tilesPerRow` beats into the fill-side FIFO.
  * The consumer dequeues one beat per tile via the `dequeue` signal from
  * the active-side FIFO.
  *
  * Address = baseAddr + rowIdx * tilesPerRow * bytesPerBeat
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

    // Avalon-MM master
    val avalon = new AvalonMMReadMaster(cfg.avalonAddrW, cfg.avalonDataW, cfg.burstCountW)

    // FIFO dequeue interface (from active-side FIFO)
    val weightData = Output(UInt(cfg.avalonDataW.W))
    val dataReady  = Output(Bool())
    val dequeue    = Input(Bool())

    // Status
    val rowDone = Output(Bool())
  })

  val sIdle :: sRead :: sFill :: Nil = Enum(3)
  val state = RegInit(sIdle)

  val bytesPerBeat = (cfg.avalonDataW / 8).U
  val addr = RegInit(0.U(cfg.avalonAddrW.W))
  val burstLen = RegInit(0.U(cfg.burstCountW.W))
  val beatsReceived = RegInit(0.U(cfg.dimW.W))

  // Active-side burst length (for rowDone tracking)
  val activeBurstLen = RegInit(0.U(cfg.burstCountW.W))
  val deqCount = RegInit(0.U(cfg.dimW.W))

  // Double-buffered FIFOs
  val maxTilesPerRow = cfg.maxDimK / cfg.numPEs
  val fifoA = Module(new Queue(UInt(cfg.avalonDataW.W), maxTilesPerRow))
  val fifoB = Module(new Queue(UInt(cfg.avalonDataW.W), maxTilesPerRow))

  // activeBuf: false = A is active (consumer), B is fill
  //            true  = B is active (consumer), A is fill
  val activeBuf = RegInit(false.B)

  // Fill-side FIFO is done when FSM returns to sIdle after a burst
  val fillDoneReg = RegInit(false.B)
  io.prefetchDone := fillDoneReg

  // Swap logic: toggle activeBuf, reset deqCount, transfer burstLen to activeBurstLen
  when(io.swap) {
    activeBuf := !activeBuf
    deqCount := 0.U
    activeBurstLen := burstLen
    fillDoneReg := false.B
  }

  // Enqueue to fill-side FIFO (driven by Avalon readdatavalid)
  val fillEnqValid = io.avalon.readdatavalid
  val fillEnqBits  = io.avalon.readdata

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

  io.rowDone := deqCount === activeBurstLen && activeBurstLen =/= 0.U

  // Clear fillDone when starting a new fill
  when(io.startRow && state === sIdle) {
    fillDoneReg := false.B
  }

  // Avalon defaults
  io.avalon.address    := addr
  io.avalon.read       := false.B
  io.avalon.burstcount := burstLen

  switch(state) {
    is(sIdle) {
      when(io.startRow) {
        val tilesPerRow = (io.dimK + (cfg.numPEs - 1).U) >> log2Ceil(cfg.numPEs).U
        addr := io.baseAddr + io.rowIdx * tilesPerRow * bytesPerBeat
        burstLen := tilesPerRow
        beatsReceived := 0.U
        state := sRead
      }
    }
    is(sRead) {
      io.avalon.read := true.B
      io.avalon.address := addr
      io.avalon.burstcount := burstLen
      when(!io.avalon.waitrequest) {
        state := sFill
      }
    }
    is(sFill) {
      when(io.avalon.readdatavalid) {
        val nextReceived = beatsReceived + 1.U
        beatsReceived := nextReceived
        when(nextReceived === burstLen) {
          fillDoneReg := true.B
          state := sIdle
        }
      }
    }
  }
}
