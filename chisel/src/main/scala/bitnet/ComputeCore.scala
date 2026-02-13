package bitnet

import chisel3._
import chisel3.util._

/** ComputeCore: wires WeightDecoder → PEArray → AdderTree → Accumulator → Requantize.
  *
  * Processes one tile (numPEs weights) per cycle through the PE array,
  * accumulates partial sums across tiles for each output row element,
  * and requantizes the final accumulated result to INT8.
  *
  * Data flow per output element result[m]:
  *   for tile in 0..tilesPerRow-1:
  *     decode weights[m][tile] → enable/sign
  *     PEArray(activations[tile*numPEs..], enable, sign) → peResults
  *     AdderTree(peResults) → tileSum  (after pipeline latency)
  *     accumulator += tileSum
  *   requantize(accumulator) → INT8 output
  */
class ComputeCore(implicit val cfg: BitNetConfig) extends Module {
  val io = IO(new Bundle {
    // Weight input (from streamer)
    val weightData  = Input(UInt(cfg.avalonDataW.W))
    val weightValid = Input(Bool())

    // Activation tile (from activation buffer)
    val activations = Input(Vec(cfg.numPEs, SInt(cfg.activationW.W)))

    // Configuration
    val dimK     = Input(UInt(cfg.dimW.W))
    val shiftAmt = Input(UInt(5.W))

    // Accumulator control
    val rowStart = Input(Bool())   // Clear accumulator for new row element
    val tileIn   = Input(Bool())   // A tile of data is being presented

    // Output
    val resultData  = Output(SInt(8.W))
    val resultValid = Output(Bool())
    val accumOut    = Output(SInt(cfg.accumW.W))  // Raw accumulator for debug
  })

  // Weight decoder
  val decoder = Module(new WeightDecoder)
  decoder.io.packed := io.weightData

  // PE array
  val peArray = Module(new PEArray)
  peArray.io.activations := io.activations
  peArray.io.enables := decoder.io.enable
  peArray.io.signs := decoder.io.sign

  // Pipeline register between PE array and adder tree.
  // Breaks the critical path: FIFO → decode → PE → adder_L0-L1
  val peRegs = Reg(Vec(cfg.numPEs, SInt(cfg.peOutW.W)))
  val peValidReg = RegInit(false.B)
  peRegs := peArray.io.results
  peValidReg := io.weightValid

  // Adder tree
  val adderTree = Module(new AdderTree)
  adderTree.io.inputs := peRegs
  adderTree.io.valid_in := peValidReg

  // Accumulator
  val accumulator = RegInit(0.S(cfg.accumW.W))

  // Flag: next valid adder tree output should replace (not add to) accumulator
  val accumReset = RegInit(true.B)

  when(io.rowStart) {
    accumReset := true.B
  }

  when(adderTree.io.valid_out) {
    when(accumReset) {
      accumulator := adderTree.io.sum
      accumReset := false.B
    }.otherwise {
      accumulator := accumulator + adderTree.io.sum
    }
  }

  io.accumOut := accumulator

  // Track when accumulation is complete for a row element
  // We count tiles going in and wait for the pipeline to flush
  val tilesPerRow = (io.dimK + (cfg.numPEs - 1).U) >> log2Ceil(cfg.numPEs).U
  val tileCount = RegInit(0.U(cfg.dimW.W))
  val tilesOutCount = RegInit(0.U(cfg.dimW.W))
  val accumulating = RegInit(false.B)

  when(io.rowStart) {
    tileCount := Mux(io.tileIn, 1.U, 0.U)
    tilesOutCount := 0.U
    accumulating := true.B
  }.elsewhen(io.tileIn) {
    tileCount := tileCount + 1.U
  }

  when(adderTree.io.valid_out && accumulating) {
    tilesOutCount := tilesOutCount + 1.U
  }

  val accumDone = accumulating && (tilesOutCount === tilesPerRow) && (tilesPerRow > 0.U)

  when(accumDone) {
    accumulating := false.B
  }

  // Requantize
  val requantize = Module(new Requantize)
  requantize.io.in := accumulator
  requantize.io.shiftAmt := io.shiftAmt
  requantize.io.valid_in := accumDone

  io.resultData := requantize.io.out
  io.resultValid := requantize.io.valid_out
}
