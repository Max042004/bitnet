package bitnet

import chisel3._
import chisel3.util._

/** Top-level BitNet accelerator integrating all sub-modules.
  *
  * Architecture:
  *   HPS ──Avalon-MM Slave──► ControlRegs ──► FSM
  *   DDR3 ◄──Avalon-MM Master── WeightStreamer ◄── FSM
  *   ActivationBuffer ──► ComputeCore (Decoder→PEArray→AdderTree→Accum→Requant) ──► ResultBuffer
  */
class BitNetAccelerator(implicit val cfg: BitNetConfig) extends Module {
  val io = IO(new Bundle {
    // Avalon-MM slave (HPS register access)
    val slave = new AvalonMMSlave()

    // Avalon-MM master (DDR3 weight reads)
    val master = new AvalonMMReadMaster(cfg.avalonAddrW, cfg.avalonDataW)
  })

  // Sub-modules
  val controlRegs = Module(new ControlRegs)
  val actBuffer   = Module(new ActivationBuffer)
  val weightStr   = Module(new WeightStreamer)
  val computeCore = Module(new ComputeCore)

  // Result buffer: stores INT8 results for readback
  val resultMem = SyncReadMem(cfg.maxDimM, SInt(8.W))
  val resultWriteIdx = RegInit(0.U(cfg.dimW.W))

  // Performance counter
  val perfCycles = RegInit(0.U(32.W))

  // ---- Connect Avalon-MM slave ----
  controlRegs.io.avalon <> io.slave

  // ---- Connect Avalon-MM master ----
  io.master <> weightStr.io.avalon

  // ---- Connect activation buffer write port from control regs ----
  actBuffer.io.writeEn   := controlRegs.io.actWriteEn
  actBuffer.io.writeAddr := controlRegs.io.actWriteAddr
  actBuffer.io.writeData := controlRegs.io.actWriteData

  // ---- Result buffer read from control regs ----
  val resReadData = resultMem.read(controlRegs.io.resReadAddr)
  controlRegs.io.resReadData := resReadData

  // ---- Main FSM ----
  val sIdle :: sLoadTile :: sWaitTile :: sStreamWeights :: sWaitPipeline :: sRowNext :: sDone :: Nil = Enum(7)
  val state = RegInit(sIdle)

  val currentRow = RegInit(0.U(cfg.dimW.W))
  val currentTile = RegInit(0.U(cfg.dimW.W))
  val tilesPerRow = RegInit(0.U(cfg.dimW.W))
  val totalRows = RegInit(0.U(cfg.dimW.W))
  val dimK = RegInit(0.U(cfg.dimW.W))
  val pipelineFlush = RegInit(0.U(4.W))

  val busy = state =/= sIdle
  val done = state === sDone

  controlRegs.io.busy := busy
  controlRegs.io.done := done
  controlRegs.io.perfCycles := perfCycles

  // Weight streamer defaults
  weightStr.io.start := false.B
  weightStr.io.baseAddr := controlRegs.io.weightBase
  weightStr.io.dimK := controlRegs.io.dimK
  weightStr.io.rowIdx := currentRow
  weightStr.io.tileIdx := currentTile

  // Activation buffer tile control defaults
  actBuffer.io.tileOffset := currentTile << log2Ceil(cfg.numPEs).U
  actBuffer.io.tileLoad := false.B

  // Compute core connections
  computeCore.io.weightData := weightStr.io.weightData
  computeCore.io.weightValid := weightStr.io.weightValid
  computeCore.io.activations := actBuffer.io.activations
  computeCore.io.dimK := dimK
  computeCore.io.shiftAmt := controlRegs.io.shiftAmt
  computeCore.io.rowStart := false.B
  computeCore.io.tileIn := false.B

  // Write results from compute core
  when(computeCore.io.resultValid) {
    resultMem.write(resultWriteIdx, computeCore.io.resultData)
    resultWriteIdx := resultWriteIdx + 1.U
  }

  // Performance counter
  when(busy && !done) {
    perfCycles := perfCycles + 1.U
  }

  switch(state) {
    is(sIdle) {
      when(controlRegs.io.start) {
        dimK := controlRegs.io.dimK
        totalRows := controlRegs.io.dimM
        tilesPerRow := (controlRegs.io.dimK + (cfg.numPEs - 1).U) >> log2Ceil(cfg.numPEs).U
        currentRow := 0.U
        currentTile := 0.U
        resultWriteIdx := 0.U
        perfCycles := 0.U
        state := sLoadTile
      }
    }
    is(sLoadTile) {
      // Request tile load from activation buffer
      actBuffer.io.tileLoad := true.B
      actBuffer.io.tileOffset := currentTile << log2Ceil(cfg.numPEs).U
      state := sWaitTile
    }
    is(sWaitTile) {
      when(actBuffer.io.tileReady) {
        // Start weight streamer for this row+tile
        state := sStreamWeights
        computeCore.io.rowStart := currentTile === 0.U

        // Issue Avalon read for this weight tile
        // Do NOT assert tileIn here — wait for weightValid in sStreamWeights
        weightStr.io.start := true.B
      }
    }
    is(sStreamWeights) {
      // Wait for weight streamer to provide data
      computeCore.io.tileIn := weightStr.io.weightValid
      when(weightStr.io.weightValid) {
        val nextTile = currentTile + 1.U
        when(nextTile >= tilesPerRow) {
          // All tiles for this row done, wait for pipeline
          pipelineFlush := 0.U
          state := sWaitPipeline
        }.otherwise {
          currentTile := nextTile
          state := sLoadTile
        }
      }
    }
    is(sWaitPipeline) {
      // Wait for adder tree pipeline + requantize to flush
      pipelineFlush := pipelineFlush + 1.U
      when(pipelineFlush >= (cfg.treePipeStages + 2).U) {
        state := sRowNext
      }
    }
    is(sRowNext) {
      val nextRow = currentRow + 1.U
      when(nextRow >= totalRows) {
        state := sDone
      }.otherwise {
        currentRow := nextRow
        currentTile := 0.U
        state := sLoadTile
      }
    }
    is(sDone) {
      state := sIdle
    }
  }
}
