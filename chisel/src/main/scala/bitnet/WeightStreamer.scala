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

/** Streams weight data from DDR3 via Avalon-MM read master.
  *
  * For each row of the output (M dimension), streams ceil(K/numPEs) beats
  * of 128-bit packed weight data. Each beat contains 64 × 2-bit weights.
  *
  * Interface:
  * - start: pulse to begin streaming for a new computation
  * - baseAddr: DDR3 base address of weight matrix
  * - dimM, dimK: matrix dimensions
  * - weightData: current 128-bit weight word (valid when weightValid)
  * - rowDone: pulses when all tiles for current row are streamed
  * - allDone: pulses when all M rows are complete
  */
class WeightStreamer(implicit val cfg: BitNetConfig) extends Module {
  val io = IO(new Bundle {
    // Control
    val start    = Input(Bool())
    val baseAddr = Input(UInt(cfg.avalonAddrW.W))
    val dimM     = Input(UInt(cfg.dimW.W))
    val dimK     = Input(UInt(cfg.dimW.W))

    // Avalon-MM master
    val avalon = new AvalonMMReadMaster(cfg.avalonAddrW, cfg.avalonDataW)

    // Output to compute core
    val weightData  = Output(UInt(cfg.avalonDataW.W))
    val weightValid = Output(Bool())
    val rowIdx      = Output(UInt(cfg.dimW.W))
    val tileIdx     = Output(UInt(cfg.dimW.W))
    val rowDone     = Output(Bool())
    val allDone     = Output(Bool())
  })

  val sIdle :: sRead :: sWaitData :: sRowDone :: sDone :: Nil = Enum(5)
  val state = RegInit(sIdle)

  val currentRow  = RegInit(0.U(cfg.dimW.W))
  val currentTile = RegInit(0.U(cfg.dimW.W))
  val tilesPerRow = RegInit(0.U(cfg.dimW.W))
  val totalRows   = RegInit(0.U(cfg.dimW.W))
  val base        = RegInit(0.U(cfg.avalonAddrW.W))

  // Bytes per weight row in DDR3: ceil(K/numPEs) * (avalonDataW/8)
  val bytesPerBeat = (cfg.avalonDataW / 8).U
  val addr = RegInit(0.U(cfg.avalonAddrW.W))

  // Defaults
  io.avalon.address := addr
  io.avalon.read := false.B
  io.weightData := 0.U
  io.weightValid := false.B
  io.rowIdx := currentRow
  io.tileIdx := currentTile
  io.rowDone := false.B
  io.allDone := false.B

  switch(state) {
    is(sIdle) {
      when(io.start) {
        totalRows := io.dimM
        // tilesPerRow = ceil(dimK / numPEs) = (dimK + numPEs - 1) / numPEs
        tilesPerRow := (io.dimK + (cfg.numPEs - 1).U) >> log2Ceil(cfg.numPEs).U
        base := io.baseAddr
        currentRow := 0.U
        currentTile := 0.U
        addr := io.baseAddr
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
        io.tileIdx := currentTile

        val nextTile = currentTile + 1.U
        when(nextTile >= tilesPerRow) {
          state := sRowDone
        }.otherwise {
          currentTile := nextTile
          addr := addr + bytesPerBeat
          state := sRead
        }
      }
    }
    is(sRowDone) {
      io.rowDone := true.B
      val nextRow = currentRow + 1.U
      when(nextRow >= totalRows) {
        state := sDone
      }.otherwise {
        currentRow := nextRow
        currentTile := 0.U
        addr := base + nextRow * tilesPerRow * bytesPerBeat
        state := sRead
      }
    }
    is(sDone) {
      io.allDone := true.B
      state := sIdle
    }
  }
}
