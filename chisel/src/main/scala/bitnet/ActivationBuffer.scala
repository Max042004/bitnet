package bitnet

import chisel3._
import chisel3.util._

/** Banked BRAM activation buffer with parallel read for the PE array.
  *
  * Uses numPEs independent BRAM banks (bank-interleaved layout) to enable
  * parallel read of all numPEs activations in 2 cycles (1 issue + 1 latency),
  * replacing the original sequential 65-cycle load.
  *
  * Bank layout: activation[i] → bank[i % numPEs] at address i / numPEs
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

  val numBanks  = cfg.numPEs
  val bankDepth = cfg.maxDimK / cfg.numPEs
  val bankSelW  = log2Ceil(numBanks)

  // 64 independent BRAM banks (bank-interleaved: act[i] in bank[i % numPEs])
  val banks = Seq.fill(numBanks)(SyncReadMem(bankDepth, SInt(cfg.activationW.W)))

  // Tile register file — holds numPEs activations for parallel read
  val tileRegs = RegInit(VecInit(Seq.fill(cfg.numPEs)(0.S(cfg.activationW.W))))

  // FSM states
  val sReady :: sLoading :: Nil = Enum(2)
  val state = RegInit(sReady)

  // --- Write path (HPS writes, bank-interleaved) ---
  // Activation i → bank[i % numPEs] at address i / numPEs
  val writeBankSel  = io.writeAddr(bankSelW - 1, 0)
  val writeBankAddr = io.writeAddr >> bankSelW

  for (b <- 0 until numBanks) {
    when(io.writeEn && writeBankSel === b.U) {
      banks(b).write(writeBankAddr, io.writeData)
    }
  }

  // --- Read path (parallel tile load, 2 cycles) ---
  // tileOffset = currentTile << log2(numPEs), so tileIndex = currentTile
  val tileIndex = io.tileOffset >> bankSelW
  val readData  = VecInit(banks.map(_.read(tileIndex)))

  io.tileReady := state === sReady

  switch(state) {
    is(sReady) {
      when(io.tileLoad) {
        state := sLoading
        // All 64 banks sample tileIndex this cycle;
        // SyncReadMem data will be valid next cycle
      }
    }
    is(sLoading) {
      // Capture SyncReadMem output (1-cycle latency from read in sReady)
      for (b <- 0 until numBanks) {
        tileRegs(b) := readData(b)
      }
      state := sReady
    }
  }

  io.activations := tileRegs
}
