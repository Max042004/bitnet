package bitnet

import chisel3._
import chisel3.util._

/** Builds T-MAC lookup tables for all groups in 4 cycles using butterfly pattern.
  *
  * For each group g of 4 activations (x0, x1, x2, x3), builds a 16-entry LUT
  * where lut[mask] = sum of activations at bit positions set in mask:
  *   lut[0]=0, lut[1]=x0, lut[2]=x1, lut[3]=x0+x1, ..., lut[15]=x0+x1+x2+x3
  *
  * All numGroups groups are built in parallel over 4 cycles (one activation per cycle).
  * LUTs are stored in registers for combinational read access during weight lookup.
  *
  * Timing: start pulse → 4 build cycles → done asserted.
  * Done is asserted the cycle after the last build cycle completes.
  */
class TmacLutBuilder(implicit val cfg: BitNetConfig) extends Module {
  val io = IO(new Bundle {
    val activations = Input(Vec(cfg.numPEs, SInt(cfg.activationW.W)))
    val start       = Input(Bool())
    val luts        = Output(Vec(cfg.numGroups, Vec(cfg.lutEntries, SInt(cfg.lutEntryW.W))))
    val done        = Output(Bool())
  })

  // LUT storage: numGroups × lutEntries × lutEntryW-bit registers
  val lutRegs = Reg(Vec(cfg.numGroups, Vec(cfg.lutEntries, SInt(cfg.lutEntryW.W))))

  // Build cycle counter (0..groupSize-1)
  val buildCycle = RegInit(0.U(log2Ceil(cfg.groupSize).W))
  val building = RegInit(false.B)
  val doneReg = RegInit(false.B)

  // Use combinational building signal so build starts on the same cycle as start
  val buildActive = building || io.start
  val effectiveCycle = Mux(io.start && !building, 0.U, buildCycle)

  when(io.start) {
    building := true.B
    buildCycle := 1.U  // Next cycle will be cycle 1 (cycle 0 runs combinationally now)
    doneReg := false.B
  }.elsewhen(building) {
    when(buildCycle === (cfg.groupSize - 1).U) {
      building := false.B
      doneReg := true.B
      buildCycle := 0.U
    }.otherwise {
      buildCycle := buildCycle + 1.U
    }
  }

  when(buildActive) {
    for (g <- 0 until cfg.numGroups) {
      // Select activation for this group at current build cycle.
      // Use a Mux tree indexed by effectiveCycle to avoid width warnings.
      val groupActs = Wire(Vec(cfg.groupSize, SInt(cfg.lutEntryW.W)))
      for (a <- 0 until cfg.groupSize) {
        groupActs(a) := io.activations(g * cfg.groupSize + a)
      }
      val act = groupActs(effectiveCycle)

      switch(effectiveCycle) {
        is(0.U) {
          lutRegs(g)(0) := 0.S
          lutRegs(g)(1) := act
        }
        is(1.U) {
          lutRegs(g)(2) := act
          lutRegs(g)(3) := lutRegs(g)(1) + act
        }
        is(2.U) {
          for (e <- 0 until 4) {
            lutRegs(g)(e + 4) := lutRegs(g)(e) + act
          }
        }
        is(3.U) {
          for (e <- 0 until 8) {
            lutRegs(g)(e + 8) := lutRegs(g)(e) + act
          }
        }
      }
    }
  }

  io.luts := lutRegs
  io.done := doneReg
}
