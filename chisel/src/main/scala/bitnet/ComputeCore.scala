package bitnet

import chisel3._

/** T-MAC ComputeCore: tile-major computation pipeline.
  *
  * Data flow per tile position k:
  *   1. Build LUTs from activations[k*128..(k+1)*128-1]  (4 cycles)
  *   2. For each row m (0..M-1):
  *      - Decode weight tile → p_mask/n_mask per group
  *      - LUT lookup: result[g] = lut[g][p_mask] - lut[g][n_mask]
  *      - 32-input adder tree → tileSum  (5-cycle pipeline)
  *      - accumArray[m] += tileSum
  *
  * The top-level FSM drives tile iteration and weight streaming.
  * ComputeCore handles LUT build + weight lookup + reduction + accumulation.
  */
class ComputeCore(implicit val cfg: BitNetConfig) extends Module {
  val io = IO(new Bundle {
    // Weight input (from streamer)
    val weightData  = Input(UInt(cfg.weightDataW.W))
    val weightValid = Input(Bool())

    // Activation tile (from activation buffer)
    val activations = Input(Vec(cfg.numPEs, SInt(cfg.activationW.W)))

    // LUT build control
    val lutBuildStart = Input(Bool())
    val lutBuildDone  = Output(Bool())

    // Row tracking (which row m the current weight belongs to)
    val rowIdx = Input(UInt(cfg.dimW.W))

    // Accumulator access
    val accumReadAddr = Input(UInt(cfg.dimW.W))
    val accumReadData = Output(SInt(cfg.accumW.W))

    // Accumulator clear
    val accumClearEn   = Input(Bool())
    val accumClearAddr = Input(UInt(cfg.dimW.W))
  })

  // ---- T-MAC LUT Builder ----
  val lutBuilder = Module(new TmacLutBuilder)
  lutBuilder.io.activations := io.activations
  lutBuilder.io.start := io.lutBuildStart
  io.lutBuildDone := lutBuilder.io.done

  // ---- T-MAC Weight Indexer ----
  val weightIndexer = Module(new TmacWeightIndexer)
  weightIndexer.io.packed := io.weightData
  weightIndexer.io.luts := lutBuilder.io.luts
  weightIndexer.io.valid_in := io.weightValid

  // ---- Adder Tree (32 inputs) ----
  val adderTree = Module(new AdderTree(cfg.numGroups, cfg.groupOutW, cfg.accumW))
  adderTree.io.inputs := weightIndexer.io.groupResults
  adderTree.io.valid_in := weightIndexer.io.valid_out

  // ---- Row index pipeline (track which row the adder tree output belongs to) ----
  // Total pipeline delay: 1 (weightIndexer reg) + tmacTreePipeStages (adder tree)
  val pipelineDelay = 1 + cfg.tmacTreePipeStages
  val rowIdxPipe = Reg(Vec(pipelineDelay, UInt(cfg.dimW.W)))

  // Shift register: stage 0 captures current rowIdx when weight is valid
  when(io.weightValid) {
    rowIdxPipe(0) := io.rowIdx
  }
  for (i <- 1 until pipelineDelay) {
    rowIdxPipe(i) := rowIdxPipe(i - 1)
  }

  // ---- Accumulator Array ----
  val accumArray = Module(new AccumulatorArray)

  // Connect RMW: adder tree output goes to accumulator
  accumArray.io.addEn   := adderTree.io.valid_out
  accumArray.io.addAddr := rowIdxPipe(pipelineDelay - 1)
  accumArray.io.addData := adderTree.io.sum

  // Direct read port for result readback
  accumArray.io.readAddr := io.accumReadAddr
  io.accumReadData := accumArray.io.readData

  // Clear control
  accumArray.io.clearEn   := io.accumClearEn
  accumArray.io.clearAddr := io.accumClearAddr
}
