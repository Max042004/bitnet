package bitnet

import chisel3._
import chisel3.util._

/** Banked BRAM activation buffer with parallel read for the PE array.
  *
  * Uses numPEs independent BRAM banks (bank-interleaved layout) to enable
  * parallel read of all numPEs activations with 1-cycle SyncReadMem latency.
  * The BRAM's internal output register provides the pipeline stage.
  *
  * Bank layout: activation[i] → bank[i % numPEs] at address i / numPEs
  */
class ActivationBuffer(implicit val cfg: BitNetConfig) extends Module {
  val actsPerBeat = (cfg.avalonDataW / cfg.activationW).max(1)  // 32 for 256-bit bus (min 1 for small test configs)

  val io = IO(new Bundle {
    // Write port (from HPS/ControlRegs)
    val writeEn   = Input(Bool())
    val writeAddr = Input(UInt(cfg.dimW.W))
    val writeData = Input(SInt(cfg.activationW.W))

    // Bulk write port (from ActivationLoader, 32 parallel writes per cycle)
    val bulkWriteEn   = Input(Bool())
    val bulkWriteData = Input(Vec(actsPerBeat, SInt(cfg.activationW.W)))
    val bulkWriteBase = Input(UInt(cfg.dimW.W))

    // Tile read address (present address cycle N, data valid cycle N+1)
    val tileOffset = Input(UInt(cfg.dimW.W))

    // Parallel read port (to PE array)
    val activations = Output(Vec(cfg.numPEs, SInt(cfg.activationW.W)))
  })

  val numBanks  = cfg.numPEs
  val bankDepth = cfg.maxDimK / cfg.numPEs
  val bankSelW  = log2Ceil(numBanks)

  // 64 independent BRAM banks (bank-interleaved: act[i] in bank[i % numPEs])
  val banks = Seq.fill(numBanks)(SyncReadMem(bankDepth, SInt(cfg.activationW.W)))

  // --- Write paths merged into single write port per bank ---
  // HPS single-element write and bulk write are mutually exclusive (HPS writes
  // before START, bulk writes during sLoadAct), so we MUX them into one write
  // port. Combined with one read port, each bank has exactly 2 ports — matching
  // Cyclone V M10K dual-port BRAM for efficient inference.

  val writeBankSel  = io.writeAddr(bankSelW - 1, 0)
  val writeBankAddr = io.writeAddr >> bankSelW

  // Read path (1-cycle pipelined via SyncReadMem)
  val tileIndex = io.tileOffset >> bankSelW

  for (b <- 0 until numBanks) {
    // Pre-decode which bulk-write index (0..actsPerBeat-1) targets this bank.
    // bulkIdx = (b - bulkWriteBase) mod numBanks. Valid when < actsPerBeat.
    val bulkIdx = (b.U(bankSelW.W) - io.bulkWriteBase(bankSelW - 1, 0))
    val bulkIdxTrunc = bulkIdx(log2Ceil(actsPerBeat) - 1, 0)
    val bulkValid = io.bulkWriteEn && bulkIdx < actsPerBeat.U
    val bulkData = io.bulkWriteData(bulkIdxTrunc)
    val bulkGlobalIdx = io.bulkWriteBase +& bulkIdx  // +& to avoid truncation
    val bulkBankAddr = bulkGlobalIdx >> bankSelW

    // Single-element HPS write targets this bank?
    val hpsValid = io.writeEn && writeBankSel === b.U

    // MUX into single write port (mutually exclusive; bulk has priority)
    val bankAddrW = log2Ceil(bankDepth)
    val wen   = hpsValid || bulkValid
    val waddr = Mux(bulkValid, bulkBankAddr(bankAddrW - 1, 0), writeBankAddr(bankAddrW - 1, 0))
    val wdata = Mux(bulkValid, bulkData, io.writeData)

    when(wen) {
      banks(b).write(waddr, wdata)
    }

    // Read port (single .read() call per bank)
    io.activations(b) := banks(b).read(tileIndex)
  }
}
