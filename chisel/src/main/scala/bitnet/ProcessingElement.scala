package bitnet

import chisel3._

/** Single DSP-free processing element for ternary weight multiplication.
  *
  * Implements: result = weight match {
  *   enable=0         → 0
  *   enable=1, sign=0 → +activation
  *   enable=1, sign=1 → -activation
  * }
  *
  * Purely combinational, ~8 ALMs on Cyclone V.
  */
class ProcessingElement(implicit val cfg: BitNetConfig) extends Module {
  val io = IO(new Bundle {
    val activation = Input(SInt(cfg.activationW.W))
    val enable     = Input(Bool())
    val sign       = Input(Bool())
    val result     = Output(SInt(cfg.peOutW.W))
  })

  val extended = Wire(SInt(cfg.peOutW.W))
  extended := io.activation

  val inverted = Wire(SInt(cfg.peOutW.W))
  inverted := -extended

  io.result := Mux(!io.enable, 0.S, Mux(io.sign, inverted, extended))
}
