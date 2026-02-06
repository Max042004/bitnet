package bitnet

import circt.stage.ChiselStage

/** Verilog generation entry point for the BitNet accelerator. */
object BitNetAccelMain extends App {
  implicit val cfg: BitNetConfig = BitNetConfig()

  ChiselStage.emitSystemVerilogFile(
    new BitNetAccelerator,
    args = Array("--target-dir", "generated"),
    firtoolOpts = Array(
      "--disable-all-randomization",
      "--strip-debug-info"
    )
  )

  println("BitNet accelerator Verilog generated in generated/")
}
