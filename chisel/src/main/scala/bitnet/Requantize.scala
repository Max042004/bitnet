package bitnet

import chisel3._

/** Requantization: arithmetic right-shift + clamp to INT8.
  *
  * output = clamp(accumulator >> shiftAmt, -128, 127)
  *
  * Single-cycle registered output.
  */
class Requantize(implicit val cfg: BitNetConfig) extends Module {
  val io = IO(new Bundle {
    val in       = Input(SInt(cfg.accumW.W))
    val shiftAmt = Input(UInt(5.W))
    val valid_in = Input(Bool())
    val out      = Output(SInt(8.W))
    val valid_out = Output(Bool())
  })

  val shifted = Wire(SInt(cfg.accumW.W))
  shifted := (io.in >> io.shiftAmt).asSInt

  val clamped = Wire(SInt(8.W))
  when(shifted > 127.S) {
    clamped := 127.S
  }.elsewhen(shifted < -128.S) {
    clamped := -128.S
  }.otherwise {
    clamped := shifted(7, 0).asSInt
  }

  io.out       := RegNext(clamped, 0.S)
  io.valid_out := RegNext(io.valid_in, false.B)
}
