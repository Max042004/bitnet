package bitnet

import chisel3._

/** Extracts p_mask/n_mask from packed weights, performs LUT lookup and subtraction.
  *
  * For each group g (0..31), extracts 4 weights from the packed 256-bit word,
  * computes 4-bit positive and negative masks, indexes into the group's LUT,
  * and outputs result[g] = lut[g][p_mask] - lut[g][n_mask].
  *
  * Weight encoding (2-bit per weight):
  *   00 → weight=0   (p=0, n=0)
  *   01 → weight=+1  (p=1, n=0)
  *   10 → weight=-1  (p=0, n=1)
  *   11 → reserved   (p=0, n=0)
  *
  * One pipeline register after subtraction for timing closure.
  */
class TmacWeightIndexer(implicit val cfg: BitNetConfig) extends Module {
  val io = IO(new Bundle {
    val packed    = Input(UInt(cfg.weightDataW.W))
    val luts      = Input(Vec(cfg.numGroups, Vec(cfg.lutEntries, SInt(cfg.lutEntryW.W))))
    val valid_in  = Input(Bool())
    val groupResults = Output(Vec(cfg.numGroups, SInt(cfg.groupOutW.W)))
    val valid_out = Output(Bool())
  })

  // Pipeline register for output
  val resultsReg = Reg(Vec(cfg.numGroups, SInt(cfg.groupOutW.W)))
  val validReg = RegInit(false.B)

  for (g <- 0 until cfg.numGroups) {
    // Extract 4 weights (8 bits) for this group
    val basebit = g * cfg.groupSize * 2

    // Build 4-bit p_mask and n_mask from the 4 weight pairs
    val pBits = Wire(Vec(cfg.groupSize, Bool()))
    val nBits = Wire(Vec(cfg.groupSize, Bool()))

    for (w <- 0 until cfg.groupSize) {
      val bit0 = io.packed(basebit + w * 2)
      val bit1 = io.packed(basebit + w * 2 + 1)
      // p_mask bit: weight = +1 → encoding 01 → bit0=1, bit1=0
      pBits(w) := bit0 & !bit1
      // n_mask bit: weight = -1 → encoding 10 → bit0=0, bit1=1
      nBits(w) := !bit0 & bit1
    }

    val pMask = pBits.asUInt
    val nMask = nBits.asUInt

    // LUT lookup: combinational read from register-based LUTs
    val pVal = Wire(SInt(cfg.groupOutW.W))
    val nVal = Wire(SInt(cfg.groupOutW.W))
    pVal := io.luts(g)(pMask)
    nVal := io.luts(g)(nMask)

    // Subtraction → pipeline register
    resultsReg(g) := pVal - nVal
  }

  validReg := io.valid_in

  io.groupResults := resultsReg
  io.valid_out := validReg
}
