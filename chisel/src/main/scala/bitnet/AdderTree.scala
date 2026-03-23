package bitnet

import chisel3._

/** Pipelined binary adder tree reducing numInputs inputs to one sum.
  *
  * Pipeline register inserted at every level to meet timing at 100 MHz.
  * Total latency = treeDepth cycles.
  * Output sign-extended to outW bits.
  *
  * @param numInputs Number of inputs to reduce (must be power of 2)
  * @param inputW    Bit width of each input (signed)
  * @param outW      Bit width of output (signed), must be >= inputW + log2(numInputs)
  */
class AdderTree(numInputs: Int, inputW: Int, outW: Int) extends Module {
  require(numInputs > 0 && (numInputs & (numInputs - 1)) == 0, "numInputs must be a power of 2")

  private val depth = (math.log(numInputs) / math.log(2)).toInt
  private val internalW = inputW + depth

  val io = IO(new Bundle {
    val inputs    = Input(Vec(numInputs, SInt(inputW.W)))
    val valid_in  = Input(Bool())
    val sum       = Output(SInt(outW.W))
    val valid_out = Output(Bool())
  })

  // Widen all inputs to internalW for headroom
  var current: Seq[SInt] = (0 until numInputs).map { i =>
    val w = Wire(SInt(internalW.W))
    w := io.inputs(i)
    w
  }

  var validPipe = io.valid_in

  for (_ <- 0 until depth) {
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

object AdderTree {
  /** Convenience constructor using BitNetConfig for T-MAC mode */
  def tmac(implicit cfg: BitNetConfig): AdderTree =
    new AdderTree(cfg.numGroups, cfg.groupOutW, cfg.accumW)
}
