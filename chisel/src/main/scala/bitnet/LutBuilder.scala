package bitnet

import chisel3._
import chisel3.util._

/** Builds T-MAC lookup tables from INT8 activations.
  *
  * For each group of 3 activations (a0, a1, a2), computes all 16 possible
  * weighted sums matching the T-MAC TL2 encoding, and writes them as a
  * 256-bit word to the correct LUT BRAM bank.
  *
  * LUT entries (matching biturbo.c tmac_build_three_lut):
  *   t[0]=0, t[1]=a2, t[2]=a1, t[3]=a0,
  *   t[4]=a1+a2, t[5]=a0+a2, t[6]=a0+a1,
  *   t[7]=a1-a2, t[8]=a0-a2, t[9]=a0-a1,
  *   t[10]=a0+a1+a2, t[11]=a0+a1-a2, t[12]=a0-a1+a2, t[13]=a0-a1-a2,
  *   t[14]=0, t[15]=0
  *
  * Pipeline: Read 3 activations (1-cycle BRAM latency) → compute 16 sums
  * (combinational) → write to LUT BRAM. Throughput: 1 group per 2 cycles
  * (due to BRAM read latency). For n3=2304 groups: ~4608 cycles = 46 us @ 100 MHz.
  */
class LutBuilder(implicit val cfg: TMacConfig) extends Module {
  val bankAddrW = log2Ceil(cfg.lutBankDepth)
  val bankSelW  = log2Ceil(cfg.numEngines)

  val io = IO(new Bundle {
    val start     = Input(Bool())
    val done      = Output(Bool())
    val numGroups = Input(UInt(cfg.dimW.W))  // n3

    // Activation buffer read ports (3 parallel reads)
    val actAddr0 = Output(UInt(cfg.dimW.W))
    val actAddr1 = Output(UInt(cfg.dimW.W))
    val actAddr2 = Output(UInt(cfg.dimW.W))
    val actData0 = Input(SInt(cfg.activationW.W))
    val actData1 = Input(SInt(cfg.activationW.W))
    val actData2 = Input(SInt(cfg.activationW.W))

    // LUT BRAM write port
    val lutWriteEn   = Output(Bool())
    val lutWriteBank = Output(UInt(bankSelW.W))
    val lutWriteAddr = Output(UInt(bankAddrW.W))
    val lutWriteData = Output(UInt(cfg.lutWordW.W))
  })

  val sIdle :: sRead :: sCompute :: sDone :: Nil = Enum(4)
  val state = RegInit(sIdle)

  val groupIdx = RegInit(0.U(cfg.dimW.W))
  val totalGroups = RegInit(0.U(cfg.dimW.W))

  // Activation read addresses (group g → activations g*3, g*3+1, g*3+2)
  val baseIdx = groupIdx * 3.U
  io.actAddr0 := baseIdx
  io.actAddr1 := baseIdx + 1.U
  io.actAddr2 := baseIdx + 2.U

  // LUT write defaults
  io.lutWriteEn   := false.B
  io.lutWriteBank := 0.U
  io.lutWriteAddr := 0.U
  io.lutWriteData := 0.U
  io.done          := false.B

  // Pipeline register for group index (to align with BRAM read latency)
  val groupIdxD1 = RegNext(groupIdx)

  switch(state) {
    is(sIdle) {
      when(io.start) {
        groupIdx := 0.U
        totalGroups := io.numGroups
        state := sRead
      }
    }
    is(sRead) {
      // Addresses are presented this cycle; data arrives next cycle
      state := sCompute
    }
    is(sCompute) {
      // BRAM data is now valid for groupIdxD1
      val a0 = io.actData0.asSInt
      val a1 = io.actData1.asSInt
      val a2 = io.actData2.asSInt

      // Widen to lutWidth for addition headroom
      val a0w = Wire(SInt(cfg.lutWidth.W)); a0w := a0
      val a1w = Wire(SInt(cfg.lutWidth.W)); a1w := a1
      val a2w = Wire(SInt(cfg.lutWidth.W)); a2w := a2

      // Compute all 16 LUT entries (pure combinational, 10 adders)
      val entries = Wire(Vec(cfg.lutEntries, SInt(cfg.lutWidth.W)))
      entries(0)  := 0.S
      entries(1)  := a2w
      entries(2)  := a1w
      entries(3)  := a0w
      entries(4)  := a1w + a2w
      entries(5)  := a0w + a2w
      entries(6)  := a0w + a1w
      entries(7)  := a1w - a2w
      entries(8)  := a0w - a2w
      entries(9)  := a0w - a1w
      entries(10) := a0w + a1w + a2w
      entries(11) := a0w + a1w - a2w
      entries(12) := a0w - a1w + a2w
      entries(13) := a0w - a1w - a2w
      entries(14) := 0.S
      entries(15) := 0.S

      // Pack into 256-bit word: entry[0] in LSB, entry[15] in MSB
      val packed = Wire(UInt(cfg.lutWordW.W))
      packed := Cat(entries.reverse.map(_.asUInt))

      // Write to LUT BRAM
      io.lutWriteEn   := true.B
      io.lutWriteBank := groupIdxD1(bankSelW - 1, 0)
      io.lutWriteAddr := (groupIdxD1 >> bankSelW.U)(bankAddrW - 1, 0)
      io.lutWriteData := packed

      // Advance to next group
      val nextGroup = groupIdxD1 + 1.U
      when(nextGroup >= totalGroups) {
        state := sDone
      }.otherwise {
        groupIdx := nextGroup
        state := sRead
      }
    }
    is(sDone) {
      io.done := true.B
      state := sIdle
    }
  }
}
