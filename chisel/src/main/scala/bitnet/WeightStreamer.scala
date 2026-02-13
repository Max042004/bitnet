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
  * On a `startRow` pulse, computes the row address and issues one burst read
  * of `tilesPerRow` beats. Incoming beats are buffered in a FIFO (Queue).
  * The consumer dequeues one beat per tile via the `dequeue` signal.
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

    // Avalon-MM master
    val avalon = new AvalonMMReadMaster(cfg.avalonAddrW, cfg.avalonDataW, cfg.burstCountW)

    // FIFO dequeue interface
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
  val deqCount = RegInit(0.U(cfg.dimW.W))

  // FIFO: depth = max tiles per row
  val maxTilesPerRow = cfg.maxDimK / cfg.numPEs
  val fifo = Module(new Queue(UInt(cfg.avalonDataW.W), maxTilesPerRow))

  // FIFO enqueue: driven by Avalon readdatavalid
  fifo.io.enq.valid := io.avalon.readdatavalid
  fifo.io.enq.bits  := io.avalon.readdata

  // FIFO dequeue: exposed to consumer
  io.weightData := fifo.io.deq.bits
  io.dataReady  := fifo.io.deq.valid
  fifo.io.deq.ready := io.dequeue

  // Track dequeues for rowDone
  when(io.startRow && state === sIdle) {
    deqCount := 0.U
  }.elsewhen(fifo.io.deq.fire) {
    deqCount := deqCount + 1.U
  }

  io.rowDone := deqCount === burstLen && burstLen =/= 0.U

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
          state := sIdle
        }
      }
    }
  }
}
