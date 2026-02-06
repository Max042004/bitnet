package bitnet

import chisel3._
import chisel3.util._

/** BRAM-backed activation buffer with parallel read for the PE array.
  *
  * Supports:
  * - Sequential write from HPS (one INT8 activation per cycle)
  * - Parallel read of numPEs activations for the current tile
  *
  * Stores up to maxDimK activations. The tile register file loads
  * numPEs activations from BRAM for the current tile offset.
  */
class ActivationBuffer(implicit val cfg: BitNetConfig) extends Module {
  val io = IO(new Bundle {
    // Write port (from HPS/ControlRegs)
    val writeEn   = Input(Bool())
    val writeAddr = Input(UInt(cfg.dimW.W))
    val writeData = Input(SInt(cfg.activationW.W))

    // Tile load control
    val tileOffset = Input(UInt(cfg.dimW.W))
    val tileLoad   = Input(Bool())
    val tileReady  = Output(Bool())

    // Parallel read port (to PE array)
    val activations = Output(Vec(cfg.numPEs, SInt(cfg.activationW.W)))
  })

  // BRAM storage for activations
  val mem = SyncReadMem(cfg.maxDimK, SInt(cfg.activationW.W))

  // Tile register file — holds numPEs activations for parallel read
  val tileRegs = RegInit(VecInit(Seq.fill(cfg.numPEs)(0.S(cfg.activationW.W))))

  // State machine for tile loading
  val sReady :: sLoading :: Nil = Enum(2)
  val state = RegInit(sReady)
  val loadIdx = RegInit(0.U(log2Ceil(cfg.numPEs + 1).W))
  val baseAddr = RegInit(0.U(cfg.dimW.W))

  // BRAM read address
  val readAddr = Wire(UInt(cfg.dimW.W))
  readAddr := baseAddr + loadIdx
  val readData = mem.read(readAddr)

  // Write port
  when(io.writeEn) {
    mem.write(io.writeAddr, io.writeData)
  }

  io.tileReady := state === sReady

  switch(state) {
    is(sReady) {
      when(io.tileLoad) {
        state := sLoading
        loadIdx := 0.U
        baseAddr := io.tileOffset
      }
    }
    is(sLoading) {
      // SyncReadMem has 1-cycle read latency, so we capture data at loadIdx-1
      when(loadIdx > 0.U) {
        tileRegs((loadIdx - 1.U)(log2Ceil(cfg.numPEs) - 1, 0)) := readData
      }
      when(loadIdx === cfg.numPEs.U) {
        state := sReady
      }.otherwise {
        loadIdx := loadIdx + 1.U
      }
    }
  }

  io.activations := tileRegs
}
