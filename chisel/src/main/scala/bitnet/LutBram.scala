package bitnet

import chisel3._
import chisel3.util._

/** Banked BRAM for T-MAC lookup tables.
  *
  * 32 banks (one per engine), each storing LUT words of 256 bits
  * (16 entries x 16-bit INT16). Group g maps to bank g % numEngines
  * at address g / numEngines.
  *
  * During LUT build phase: write port active (1 group per cycle).
  * During GEMV compute phase: read port active (32 banks read in parallel).
  *
  * Each bank uses dual-port M10K: 1 write + 1 read port.
  */
class LutBram(implicit val cfg: TMacConfig) extends Module {
  val bankAddrW = log2Ceil(cfg.lutBankDepth)

  val io = IO(new Bundle {
    // Write port (from LutBuilder, during build phase)
    val writeEn   = Input(Bool())
    val writeBank = Input(UInt(log2Ceil(cfg.numEngines).W))
    val writeAddr = Input(UInt(bankAddrW.W))
    val writeData = Input(UInt(cfg.lutWordW.W)) // 256 bits: all 16 LUT entries

    // Read ports (from compute engines, during GEMV phase)
    // All 32 banks read simultaneously, each at its own address
    val readAddr  = Input(Vec(cfg.numEngines, UInt(bankAddrW.W)))
    val readData  = Output(Vec(cfg.numEngines, UInt(cfg.lutWordW.W)))
  })

  val banks = Seq.fill(cfg.numEngines)(SyncReadMem(cfg.lutBankDepth, UInt(cfg.lutWordW.W)))

  for (b <- 0 until cfg.numEngines) {
    // Write port: only the targeted bank writes
    when(io.writeEn && io.writeBank === b.U) {
      banks(b).write(io.writeAddr, io.writeData)
    }

    // Read port: each bank always reads (address from compute engine)
    io.readData(b) := banks(b).read(io.readAddr(b))
  }
}
