package bitnet

import circt.stage.ChiselStage

/** Verilog generation entry point for the T-MAC accelerator. */
object TMacAccelMain extends App {
  implicit val cfg: TMacConfig = TMacConfig()

  ChiselStage.emitSystemVerilogFile(
    new TMacAccelerator,
    args = Array("--target-dir", "generated"),
    firtoolOpts = Array(
      "--disable-all-randomization",
      "--strip-debug-info"
    )
  )

  println("T-MAC accelerator Verilog generated in generated/")
}
