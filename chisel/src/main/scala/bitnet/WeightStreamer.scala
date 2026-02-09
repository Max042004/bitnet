package bitnet

import chisel3._
import chisel3.util._

/** Avalon-MM read-master interface bundle. */
class AvalonMMReadMaster(addrW: Int, dataW: Int) extends Bundle {
  val address    = Output(UInt(addrW.W))
  val read       = Output(Bool())
  val readdata   = Input(UInt(dataW.W))
  val waitrequest = Input(Bool())
  val readdatavalid = Input(Bool())
}

/** Fetches one tile of weight data from DDR3 via Avalon-MM read master.
  *
  * The parent FSM controls row/tile iteration. Each start pulse fetches
  * a single 128-bit beat at the address computed from baseAddr, rowIdx,
  * tileIdx, and dimK.
  *
  * Address = baseAddr + (rowIdx * tilesPerRow + tileIdx) * bytesPerBeat
  * where tilesPerRow = ceil(dimK / numPEs).
  */
class WeightStreamer(implicit val cfg: BitNetConfig) extends Module {
  val io = IO(new Bundle {
    // Control
    val start    = Input(Bool())
    val baseAddr = Input(UInt(cfg.avalonAddrW.W))
    val dimK     = Input(UInt(cfg.dimW.W))
    val rowIdx   = Input(UInt(cfg.dimW.W))
    val tileIdx  = Input(UInt(cfg.dimW.W))

    // Avalon-MM master
    val avalon = new AvalonMMReadMaster(cfg.avalonAddrW, cfg.avalonDataW)

    // Output to compute core
    val weightData  = Output(UInt(cfg.avalonDataW.W))
    val weightValid = Output(Bool())
  })

  val sIdle :: sRead :: sWaitData :: Nil = Enum(3)
  val state = RegInit(sIdle)

  val bytesPerBeat = (cfg.avalonDataW / 8).U
  val addr = RegInit(0.U(cfg.avalonAddrW.W))

  // Defaults
  io.avalon.address := addr
  io.avalon.read := false.B
  io.weightData := 0.U
  io.weightValid := false.B

  switch(state) {
    is(sIdle) {
      when(io.start) {
        val tilesPerRow = (io.dimK + (cfg.numPEs - 1).U) >> log2Ceil(cfg.numPEs).U
        addr := io.baseAddr + (io.rowIdx * tilesPerRow + io.tileIdx) * bytesPerBeat
        state := sRead
      }
    }
    is(sRead) {
      io.avalon.read := true.B
      io.avalon.address := addr
      when(!io.avalon.waitrequest) {
        state := sWaitData
      }
    }
    is(sWaitData) {
      when(io.avalon.readdatavalid) {
        io.weightData := io.avalon.readdata
        io.weightValid := true.B
        state := sIdle
      }
    }
  }
}
