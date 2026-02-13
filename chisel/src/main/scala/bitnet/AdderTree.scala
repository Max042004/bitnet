package bitnet

import chisel3._

/** Pipelined binary adder tree reducing numPEs inputs to one sum.
  *
  * 128 [REG] →64 [REG] →32 [REG] →16 [REG] →8 [REG] →4 [REG] →2 [REG] →1
  *
  * Pipeline register inserted at every level to meet timing at 100 MHz.
  * Total latency = treeDepth cycles.
  * Output sign-extended to accumW bits.
  */
class AdderTree(implicit val cfg: BitNetConfig) extends Module {
  val io = IO(new Bundle {
    val inputs   = Input(Vec(cfg.numPEs, SInt(cfg.peOutW.W)))
    val valid_in = Input(Bool())
    val sum      = Output(SInt(cfg.accumW.W))
    val valid_out = Output(Bool())
  })

  // Widen all inputs to treeOutW for headroom
  var current: Seq[SInt] = (0 until cfg.numPEs).map { i =>
    val w = Wire(SInt(cfg.treeOutW.W))
    w := io.inputs(i)
    w
  }

  var validPipe = io.valid_in

  for (_ <- 0 until cfg.treeDepth) {
    val half = current.length / 2
    val next = (0 until half).map { i =>
      current(2 * i) +& current(2 * i + 1)
    }

    // Pipeline register at every level for timing closure at 100 MHz
    current = next.map(x => RegNext(x))
    validPipe = RegNext(validPipe, false.B)
  }

  io.sum := current.head
  io.valid_out := validPipe
}
