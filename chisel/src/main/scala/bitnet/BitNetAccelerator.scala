package bitnet

import chisel3._
import chisel3.util._

/** Avalon-MM read+write master interface bundle with burst support. */
class AvalonMMMaster(addrW: Int, dataW: Int, burstCountW: Int) extends Bundle {
  val address       = Output(UInt(addrW.W))
  val read          = Output(Bool())
  val write         = Output(Bool())
  val writedata     = Output(UInt(dataW.W))
  val byteenable    = Output(UInt((dataW / 8).W))
  val readdata      = Input(UInt(dataW.W))
  val waitrequest   = Input(Bool())
  val readdatavalid = Input(Bool())
  val burstcount    = Output(UInt(burstCountW.W))
}

/** Top-level BitNet accelerator with T-MAC tile-major computation.
  *
  * Architecture:
  *   HPS ──Avalon-MM Slave──► ControlRegs ──► FSM
  *   DDR3 ◄──Avalon-MM Master── WeightStreamer / ActivationLoader / ResultWriter ◄── FSM
  *   ActivationBuffer ──► ComputeCore (LutBuilder→WeightIndexer→AdderTree→AccumArray) ──► ResultBuffer
  *
  * T-MAC tile-major order: for each tile position k, build LUTs once from
  * activations, then stream M weight tiles through the lookup pipeline.
  * LUT build cost (4 cycles) is amortized over M rows.
  *
  * Weight layout in DDR3 (tile-major):
  *   [tile0_row0][tile0_row1]...[tile0_rowM-1][tile1_row0]...
  */
class BitNetAccelerator(implicit val cfg: BitNetConfig) extends Module {
  val io = IO(new Bundle {
    val slave  = new AvalonMMSlave()
    val master = new AvalonMMMaster(cfg.avalonAddrW, cfg.avalonDataW, cfg.burstCountW)
  })

  // Sub-modules
  val controlRegs = Module(new ControlRegs)
  val actBuffer   = Module(new ActivationBuffer)
  val weightStr   = Module(new WeightStreamer)
  val computeCore = Module(new ComputeCore)
  val actLoader   = Module(new ActivationLoader)
  val resWriter   = Module(new ResultWriter)

  // Result buffer: stores raw accumulator values for readback
  val resultMem = SyncReadMem(cfg.maxDimM, SInt(32.W))
  val resultWriteIdx = RegInit(0.U(cfg.dimW.W))

  // Performance counter
  val perfCycles = RegInit(0.U(32.W))

  // ---- Connect Avalon-MM slave ----
  controlRegs.io.avalon <> io.slave

  // ---- Connect activation buffer write port from control regs ----
  actBuffer.io.writeEn   := controlRegs.io.actWriteEn
  actBuffer.io.writeAddr := controlRegs.io.actWriteAddr
  actBuffer.io.writeData := controlRegs.io.actWriteData

  // ---- Connect ActivationLoader → ActivationBuffer bulk write ----
  actBuffer.io.bulkWriteEn   := actLoader.io.bulkWriteEn
  actBuffer.io.bulkWriteData := actLoader.io.bulkWriteData
  actBuffer.io.bulkWriteBase := actLoader.io.bulkWriteBase

  // ---- Result buffer read from control regs ----
  val resReadAddr = Wire(UInt(cfg.dimW.W))
  val resReadData = resultMem.read(resReadAddr)
  controlRegs.io.resReadData := resReadData

  // ---- Main FSM ----
  val sIdle :: sLoadAct :: sClearAccum :: sPresentTile :: sBuildLut :: sStartStream :: sStreamWeights :: sWaitFlush :: sTileNext :: sCopyResults :: sWriteResults :: sDone :: Nil = Enum(12)
  val state = RegInit(sIdle)

  val currentTile = RegInit(0.U(cfg.dimW.W))
  val currentRow = RegInit(0.U(cfg.dimW.W))
  val tilesPerRow = RegInit(0.U(cfg.dimW.W))
  val totalRows = RegInit(0.U(cfg.dimW.W))
  val dimK = RegInit(0.U(cfg.dimW.W))
  val pipelineFlush = RegInit(0.U(4.W))
  val ddr3Mode = RegInit(false.B)
  val clearIdx = RegInit(0.U(cfg.dimW.W))
  val copyIdx = RegInit(0.U(cfg.dimW.W))
  val copyReadPending = RegInit(false.B)

  val busy = state =/= sIdle
  val done = state === sDone

  controlRegs.io.busy := busy
  controlRegs.io.done := done
  controlRegs.io.perfCycles := perfCycles

  // ---- Weight streamer defaults ----
  weightStr.io.startTile  := false.B
  weightStr.io.baseAddr   := controlRegs.io.weightBase
  weightStr.io.dimM       := totalRows
  weightStr.io.tileIdx    := currentTile
  weightStr.io.tileStride := controlRegs.io.tileStride
  weightStr.io.dequeue    := false.B

  // ---- ActivationLoader defaults ----
  actLoader.io.start    := false.B
  actLoader.io.ddr3Addr := controlRegs.io.actDdr3Base
  actLoader.io.dimK     := dimK

  // ---- ResultWriter defaults ----
  resWriter.io.start    := false.B
  resWriter.io.ddr3Addr := controlRegs.io.resDdr3Base
  resWriter.io.dimM     := totalRows

  // ResultWriter reads from resultMem
  resWriter.io.resReadData := resReadData

  // ResultMem read address mux: ResultWriter during sWriteResults, ControlRegs otherwise
  resReadAddr := Mux(state === sWriteResults, resWriter.io.resReadAddr, controlRegs.io.resReadAddr)

  // ---- Activation buffer tile address ----
  actBuffer.io.tileOffset := currentTile << log2Ceil(cfg.numPEs).U

  // ---- ComputeCore connections ----
  computeCore.io.weightData  := weightStr.io.weightData
  computeCore.io.weightValid := false.B
  computeCore.io.activations := actBuffer.io.activations
  computeCore.io.lutBuildStart := false.B
  computeCore.io.rowIdx      := currentRow
  computeCore.io.accumReadAddr := Mux(state === sCopyResults, copyIdx, 0.U)
  computeCore.io.accumClearEn   := false.B
  computeCore.io.accumClearAddr := clearIdx

  // Performance counter
  when(busy && !done) {
    perfCycles := perfCycles + 1.U
  }

  // ---- Master port mux ----
  io.master.address    := 0.U
  io.master.read       := false.B
  io.master.write      := false.B
  io.master.writedata  := 0.U
  io.master.byteenable := 0.U
  io.master.burstcount := 0.U

  // ActivationLoader Avalon signals
  actLoader.io.avalon_waitrequest   := true.B
  actLoader.io.avalon_readdatavalid := false.B
  actLoader.io.avalon_readdata      := 0.U

  // WeightStreamer Avalon signals
  weightStr.io.avalon.waitrequest   := true.B
  weightStr.io.avalon.readdatavalid := false.B
  weightStr.io.avalon.readdata      := 0.U

  // ResultWriter Avalon signals
  resWriter.io.avalon_waitrequest := true.B

  when(state === sLoadAct) {
    io.master.address   := actLoader.io.avalon_address
    io.master.read      := actLoader.io.avalon_read
    io.master.burstcount := actLoader.io.avalon_burstcount
    actLoader.io.avalon_waitrequest   := io.master.waitrequest
    actLoader.io.avalon_readdatavalid := io.master.readdatavalid
    actLoader.io.avalon_readdata      := io.master.readdata
  }.elsewhen(state === sWriteResults) {
    io.master.address    := resWriter.io.avalon_address
    io.master.write      := resWriter.io.avalon_write
    io.master.writedata  := resWriter.io.avalon_writedata
    io.master.byteenable := resWriter.io.avalon_byteenable
    io.master.burstcount := resWriter.io.avalon_burstcount
    resWriter.io.avalon_waitrequest := io.master.waitrequest
  }.otherwise {
    // WeightStreamer drives master reads (during compute states)
    io.master.address   := weightStr.io.avalon.address
    io.master.read      := weightStr.io.avalon.read
    io.master.burstcount := weightStr.io.avalon.burstcount
    weightStr.io.avalon.waitrequest   := io.master.waitrequest
    weightStr.io.avalon.readdatavalid := io.master.readdatavalid
    weightStr.io.avalon.readdata      := io.master.readdata
  }

  // ---- FSM ----
  switch(state) {
    is(sIdle) {
      when(controlRegs.io.start) {
        dimK := controlRegs.io.dimK
        totalRows := controlRegs.io.dimM
        tilesPerRow := (controlRegs.io.dimK + (cfg.numPEs - 1).U) >> log2Ceil(cfg.numPEs).U
        currentTile := 0.U
        currentRow := 0.U
        perfCycles := 0.U
        ddr3Mode := controlRegs.io.ddr3Mode
        when(controlRegs.io.ddr3Mode) {
          state := sLoadAct
        }.otherwise {
          state := sClearAccum
          clearIdx := 0.U
        }
      }
    }

    is(sLoadAct) {
      actLoader.io.start := !actLoader.io.done && (RegNext(state) =/= sLoadAct)
      when(actLoader.io.done) {
        state := sClearAccum
        clearIdx := 0.U
      }
    }

    is(sClearAccum) {
      // Clear all M accumulator entries to zero
      computeCore.io.accumClearEn := true.B
      computeCore.io.accumClearAddr := clearIdx
      clearIdx := clearIdx + 1.U
      when(clearIdx >= totalRows - 1.U) {
        state := sPresentTile
      }
    }

    is(sPresentTile) {
      // Present tile address to activation buffer (SyncReadMem: data valid next cycle)
      actBuffer.io.tileOffset := currentTile << log2Ceil(cfg.numPEs).U
      state := sBuildLut
    }

    is(sBuildLut) {
      // Activations are now valid from the BRAM read.
      // Start LUT builder (takes 4 cycles)
      computeCore.io.lutBuildStart := RegNext(state) =/= sBuildLut
      when(computeCore.io.lutBuildDone) {
        state := sStartStream
      }
    }

    is(sStartStream) {
      // Pulse startTile to begin DDR3 weight fetch for this tile position
      weightStr.io.startTile := true.B
      weightStr.io.tileIdx := currentTile
      currentRow := 0.U
      state := sStreamWeights
    }

    is(sStreamWeights) {
      // Dequeue weight tiles one per cycle, feed to compute core
      when(weightStr.io.dataReady) {
        weightStr.io.dequeue := true.B
        computeCore.io.weightValid := true.B
        computeCore.io.rowIdx := currentRow
        val nextRow = currentRow + 1.U
        when(nextRow >= totalRows) {
          // All rows for this tile done
          pipelineFlush := 0.U
          state := sWaitFlush
        }.otherwise {
          currentRow := nextRow
        }
      }
    }

    is(sWaitFlush) {
      // Wait for adder tree pipeline + accumulator write-back to drain
      // Pipeline delay: 1 (weightIndexer) + tmacTreePipeStages (adder tree) + 1 (accum RMW)
      pipelineFlush := pipelineFlush + 1.U
      when(pipelineFlush >= (cfg.tmacTreePipeStages + 3).U) {
        state := sTileNext
      }
    }

    is(sTileNext) {
      val nextTile = currentTile + 1.U
      when(nextTile >= tilesPerRow) {
        // All tiles done → copy results
        state := sCopyResults
        copyIdx := 0.U
        copyReadPending := false.B
        resultWriteIdx := 0.U
      }.otherwise {
        currentTile := nextTile
        state := sPresentTile
      }
    }

    is(sCopyResults) {
      // Copy M accumulators from AccumulatorArray → resultMem
      // AccumulatorArray uses SyncReadMem: present addr cycle N, data available cycle N+1.
      // copyIdx tracks the address to present; copyReadPending means data is available.
      // Present next read address early so data is ready next cycle.
      computeCore.io.accumReadAddr := copyIdx

      when(!copyReadPending) {
        // First cycle: just present addr 0, data not yet valid
        copyReadPending := true.B
        copyIdx := copyIdx + 1.U  // Advance so next cycle presents addr 1
      }.otherwise {
        // Data from previous cycle's address is now valid
        val accumWide = Wire(SInt(32.W))
        accumWide := computeCore.io.accumReadData
        resultMem.write(resultWriteIdx, accumWide)
        resultWriteIdx := resultWriteIdx + 1.U

        when(resultWriteIdx + 1.U >= totalRows) {
          when(ddr3Mode) {
            state := sWriteResults
          }.otherwise {
            state := sDone
          }
        }.otherwise {
          copyIdx := copyIdx + 1.U  // Present next address
        }
      }
    }

    is(sWriteResults) {
      resWriter.io.start := !resWriter.io.done && (RegNext(state) =/= sWriteResults)
      when(resWriter.io.done) {
        state := sDone
      }
    }

    is(sDone) {
      state := sIdle
    }
  }
}
