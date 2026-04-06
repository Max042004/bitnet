package bitnet

import chisel3._
import chisel3.util._

/** Activation buffer for T-MAC accelerator.
  *
  * 16-bank BRAM (matching actsPerBeat=16 for efficient DDR3 bulk writes).
  * Supports 3 parallel reads for LUT builder: since gcd(3, 16) = 1,
  * three consecutive activation indices never map to the same bank,
  * so all 3 reads can be served simultaneously without conflicts.
  *
  * Bank layout: activation[i] → bank[i % 16] at address i / 16
  */
class TMacActivationBuffer(implicit val cfg: TMacConfig) extends Module {
  val numBanks = cfg.numActBanks     // 16
  val bankDepth = cfg.maxDimK / numBanks
  val bankSelW = log2Ceil(numBanks)  // 4
  val bankAddrW = log2Ceil(bankDepth)

  val io = IO(new Bundle {
    // HPS single-element write
    val writeEn   = Input(Bool())
    val writeAddr = Input(UInt(cfg.dimW.W))
    val writeData = Input(SInt(cfg.activationW.W))

    // Bulk write from ActivationLoader (actsPerBeat values per cycle)
    val bulkWriteEn   = Input(Bool())
    val bulkWriteData = Input(Vec(cfg.actsPerBeat, SInt(cfg.activationW.W)))
    val bulkWriteBase = Input(UInt(cfg.dimW.W))

    // 3 parallel reads for LUT builder (1-cycle latency via SyncReadMem)
    val readAddr0 = Input(UInt(cfg.dimW.W))
    val readAddr1 = Input(UInt(cfg.dimW.W))
    val readAddr2 = Input(UInt(cfg.dimW.W))
    val readData0 = Output(SInt(cfg.activationW.W))
    val readData1 = Output(SInt(cfg.activationW.W))
    val readData2 = Output(SInt(cfg.activationW.W))
  })

  val banks = Seq.fill(numBanks)(SyncReadMem(bankDepth, SInt(cfg.activationW.W)))

  // --- Write path ---
  // Bulk writes and HPS writes are mutually exclusive (different FSM states).
  // Bulk write: all 16 banks get one value each (base is always 16-aligned from ActivationLoader).
  // HPS write: one bank gets one value.

  for (b <- 0 until numBanks) {
    val bulkGlobalIdx = io.bulkWriteBase + b.U
    val bulkBankAddr = bulkGlobalIdx >> bankSelW.U

    val hpsBankSel = io.writeAddr(bankSelW - 1, 0)
    val hpsBankAddr = io.writeAddr >> bankSelW.U

    val wen = (io.bulkWriteEn) || (io.writeEn && hpsBankSel === b.U)
    val waddr = Mux(io.bulkWriteEn, bulkBankAddr(bankAddrW - 1, 0), hpsBankAddr(bankAddrW - 1, 0))
    val wdata = Mux(io.bulkWriteEn, io.bulkWriteData(b), io.writeData)

    when(wen) {
      banks(b).write(waddr, wdata)
    }
  }

  // --- Read path ---
  // Route each of the 3 read addresses to the correct bank.
  // Since gcd(3, 16) = 1, three consecutive indices never collide on the same bank.
  // Each bank has exactly 1 read port; we MUX the address from whichever
  // of the 3 read ports targets that bank.

  val bank0 = io.readAddr0(bankSelW - 1, 0)
  val bank1 = io.readAddr1(bankSelW - 1, 0)
  val bank2 = io.readAddr2(bankSelW - 1, 0)
  val addr0 = io.readAddr0 >> bankSelW.U
  val addr1 = io.readAddr1 >> bankSelW.U
  val addr2 = io.readAddr2 >> bankSelW.U

  val bankReadData = Wire(Vec(numBanks, SInt(cfg.activationW.W)))

  for (b <- 0 until numBanks) {
    // MUX read address: at most one of the 3 ports targets this bank
    val readAddr = Wire(UInt(bankAddrW.W))
    readAddr := addr0(bankAddrW - 1, 0) // default
    when(bank1 === b.U) { readAddr := addr1(bankAddrW - 1, 0) }
    when(bank2 === b.U) { readAddr := addr2(bankAddrW - 1, 0) }
    when(bank0 === b.U) { readAddr := addr0(bankAddrW - 1, 0) }

    bankReadData(b) := banks(b).read(readAddr)
  }

  // Output MUX: select correct bank's data for each read port.
  // Use RegNext on bank select since SyncReadMem has 1-cycle latency.
  val bank0Reg = RegNext(bank0)
  val bank1Reg = RegNext(bank1)
  val bank2Reg = RegNext(bank2)

  io.readData0 := bankReadData(bank0Reg)
  io.readData1 := bankReadData(bank1Reg)
  io.readData2 := bankReadData(bank2Reg)
}
