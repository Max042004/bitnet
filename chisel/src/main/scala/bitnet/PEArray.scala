package bitnet

import chisel3._

/** Array of numPEs processing elements operating in parallel.
  *
  * All PEs share the same activation input but receive individual enable/sign
  * control signals from the weight decoder.
  */
class PEArray(implicit val cfg: BitNetConfig) extends Module {
  val io = IO(new Bundle {
    val activations = Input(Vec(cfg.numPEs, SInt(cfg.activationW.W)))
    val enables     = Input(Vec(cfg.numPEs, Bool()))
    val signs       = Input(Vec(cfg.numPEs, Bool()))
    val results     = Output(Vec(cfg.numPEs, SInt(cfg.peOutW.W)))
  })

  val pes = Seq.fill(cfg.numPEs)(Module(new ProcessingElement))

  for (i <- 0 until cfg.numPEs) {
    pes(i).io.activation := io.activations(i)
    pes(i).io.enable     := io.enables(i)
    pes(i).io.sign       := io.signs(i)
    io.results(i)        := pes(i).io.result
  }
}
