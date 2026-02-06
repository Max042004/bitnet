package bitnet

import chisel3._

/** Decodes packed 2-bit weight data into enable/sign control signals.
  *
  * Weight encoding (matches reference):
  *   00 → weight=0   (enable=0, sign=X)
  *   01 → weight=+1  (enable=1, sign=0)
  *   10 → weight=-1  (enable=1, sign=1)
  *   11 → reserved   (enable=0, sign=X)
  *
  * enable = bit0 XOR bit1
  * sign   = bit1
  *
  * Purely combinational. Decodes numPEs × 2-bit fields from a wide input word.
  */
class WeightDecoder(implicit val cfg: BitNetConfig) extends Module {
  val io = IO(new Bundle {
    val packed = Input(UInt(cfg.avalonDataW.W))
    val enable = Output(Vec(cfg.numPEs, Bool()))
    val sign   = Output(Vec(cfg.numPEs, Bool()))
  })

  for (i <- 0 until cfg.numPEs) {
    val bit0 = io.packed(i * 2)
    val bit1 = io.packed(i * 2 + 1)
    io.enable(i) := bit0 ^ bit1
    io.sign(i)   := bit1
  }
}
