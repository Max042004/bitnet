package bitnet

import chisel3._
import chisel3.util._

/** Top-level T-MAC FPGA accelerator.
  *
  * Architecture:
  *   HPS ──Avalon-MM Slave──► TMacControlRegs ──► FSM
  *   DDR3 ◄──Avalon-MM Master── TMacWeightStreamer / TMacActivationLoader / TMacResultWriter ◄── FSM
  *   TMacActivationBuffer ──► LutBuilder ──► LutBram ──► TMacComputeCore ──► ResultMem
  *
  * Two-phase operation per GEMV:
  *   1. LUT Build: Read activations, compute 16-entry LUT per 3-weight group
  *   2. GEMV: Stream weights from DDR3, lookup + accumulate across rows
  *
  * Weight prefetch: double-buffered TMacWeightStreamer overlaps DDR3 reads
  * for row N+1 with compute for row N.
  *
  * DDR3 mode: activations loaded from DDR3 via TMacActivationLoader,
  * results written to DDR3 via TMacResultWriter.
  */
class TMacAccelerator(implicit val cfg: TMacConfig) extends Module {
  val io = IO(new Bundle {
    val slave  = new AvalonMMSlave()
    val master = new AvalonMMMaster(cfg.avalonAddrW, cfg.avalonDataW, cfg.burstCountW)
  })

  // Sub-modules
  val controlRegs = Module(new TMacControlRegs)
  val actBuffer   = Module(new TMacActivationBuffer)
  val actLoader   = Module(new TMacActivationLoader)
  val lutBram     = Module(new LutBram)
  val lutBuilder  = Module(new LutBuilder)
  val weightStr   = Module(new TMacWeightStreamer)
  val computeCore = Module(new TMacComputeCore)
  val resWriter   = Module(new TMacResultWriter)

  // Result buffer: stores raw INT32 accumulator values
  val resultMem = SyncReadMem(cfg.maxDimM, SInt(32.W))
  val resultWriteIdx = RegInit(0.U(cfg.dimW.W))

  // Performance counter
  val perfCycles = RegInit(0.U(32.W))

  // ---- Connect Avalon-MM slave ----
  controlRegs.io.avalon <> io.slave

  // ---- Connect activation buffer write from control regs ----
  actBuffer.io.writeEn   := controlRegs.io.actWriteEn
  actBuffer.io.writeAddr := controlRegs.io.actWriteAddr
  actBuffer.io.writeData := controlRegs.io.actWriteData

  // ---- Connect ActivationLoader → ActivationBuffer bulk write ----
  actBuffer.io.bulkWriteEn   := actLoader.io.bulkWriteEn
  actBuffer.io.bulkWriteData := actLoader.io.bulkWriteData
  actBuffer.io.bulkWriteBase := actLoader.io.bulkWriteBase

  // ---- Connect LutBuilder ↔ ActivationBuffer read ports ----
  actBuffer.io.readAddr0 := lutBuilder.io.actAddr0
  actBuffer.io.readAddr1 := lutBuilder.io.actAddr1
  actBuffer.io.readAddr2 := lutBuilder.io.actAddr2
  lutBuilder.io.actData0 := actBuffer.io.readData0
  lutBuilder.io.actData1 := actBuffer.io.readData1
  lutBuilder.io.actData2 := actBuffer.io.readData2

  // ---- Connect LutBuilder → LutBram write ----
  lutBram.io.writeEn   := lutBuilder.io.lutWriteEn
  lutBram.io.writeBank := lutBuilder.io.lutWriteBank
  lutBram.io.writeAddr := lutBuilder.io.lutWriteAddr
  lutBram.io.writeData := lutBuilder.io.lutWriteData

  // ---- Connect TMacComputeCore ↔ LutBram read ----
  lutBram.io.readAddr := computeCore.io.lutReadAddr
  computeCore.io.lutReadData := lutBram.io.readData

  // ---- Result buffer ----
  val resReadAddr = Wire(UInt(cfg.dimW.W))
  val resReadData = resultMem.read(resReadAddr)
  controlRegs.io.resReadData := resReadData

  // Write raw accumulator to result buffer
  val accumWide = Wire(SInt(32.W))
  accumWide := computeCore.io.accumOut
  when(computeCore.io.resultValid) {
    resultMem.write(resultWriteIdx, accumWide)
    resultWriteIdx := resultWriteIdx + 1.U
  }

  // ---- Main FSM ----
  val sIdle :: sLoadAct :: sLutBuild :: sStartRow :: sWaitFill :: sSwapAndGo :: sStartPrefetch :: sLoadTile :: sConsumeWeight :: sWaitPipeline :: sRowNext :: sWriteResults :: sDone :: Nil = Enum(13)
  val state = RegInit(sIdle)

  val currentRow = RegInit(0.U(cfg.dimW.W))
  val currentTile = RegInit(0.U(cfg.dimW.W))
  val tilesPerRow = RegInit(0.U(cfg.dimW.W))
  val totalRows = RegInit(0.U(cfg.dimW.W))
  val dimK = RegInit(0.U(cfg.dimW.W))
  val numGroups = RegInit(0.U(cfg.dimW.W))
  val pipelineFlush = RegInit(0.U(4.W))
  val ddr3Mode = RegInit(false.B)

  // Pipeline depth: 1 (BRAM latency for nibData/signData) + 1 (nibble/sign pipeline reg)
  //                + 1 (engine pipeline reg) + treeDepth (adder tree) + 1 (accum)
  val totalPipeLatency = (1 + 1 + 1 + cfg.treeDepth + 1).U

  val busy = state =/= sIdle
  val done = state === sDone

  controlRegs.io.busy := busy
  controlRegs.io.done := done
  controlRegs.io.perfCycles := perfCycles

  // ---- Weight streamer defaults ----
  weightStr.io.startRow := false.B
  weightStr.io.nibBase  := controlRegs.io.nibBase
  weightStr.io.signBase := controlRegs.io.signBase
  weightStr.io.dimK     := dimK
  weightStr.io.rowIdx   := currentRow
  weightStr.io.swap     := false.B
  weightStr.io.tileIdx  := currentTile

  // ---- LutBuilder defaults ----
  lutBuilder.io.start     := false.B
  lutBuilder.io.numGroups := numGroups

  // ---- ActivationLoader defaults ----
  actLoader.io.start    := false.B
  actLoader.io.ddr3Addr := controlRegs.io.actDdr3Base
  actLoader.io.dimK     := dimK

  // ---- ResultWriter defaults ----
  resWriter.io.start    := false.B
  resWriter.io.ddr3Addr := controlRegs.io.resDdr3Base
  resWriter.io.dimM     := totalRows

  resWriter.io.resReadData := resReadData
  resReadAddr := Mux(state === sWriteResults, resWriter.io.resReadAddr, controlRegs.io.resReadAddr)

  // ---- ComputeCore defaults ----
  computeCore.io.nibbles   := 0.U
  computeCore.io.signs     := 0.U
  computeCore.io.tileValid := false.B
  computeCore.io.tileIdx   := currentTile
  computeCore.io.numGroups := numGroups
  computeCore.io.rowStart  := false.B
  computeCore.io.tileIn    := false.B

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
    io.master.address     := resWriter.io.avalon_address
    io.master.write       := resWriter.io.avalon_write
    io.master.writedata   := resWriter.io.avalon_writedata
    io.master.byteenable  := resWriter.io.avalon_byteenable
    io.master.burstcount  := resWriter.io.avalon_burstcount
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
        val n3 = controlRegs.io.dimK / cfg.groupSize.U
        numGroups := n3
        tilesPerRow := (n3 + (cfg.numEngines - 1).U) / cfg.numEngines.U
        currentRow := 0.U
        currentTile := 0.U
        resultWriteIdx := 0.U
        perfCycles := 0.U
        ddr3Mode := controlRegs.io.ddr3Mode
        when(controlRegs.io.ddr3Mode) {
          state := sLoadAct
        }.otherwise {
          state := sLutBuild
        }
      }
    }

    is(sLoadAct) {
      actLoader.io.start := !actLoader.io.done && (RegNext(state) =/= sLoadAct)
      when(actLoader.io.done) {
        state := sLutBuild
      }
    }

    is(sLutBuild) {
      // Pulse start on entry
      lutBuilder.io.start := RegNext(state) =/= sLutBuild
      when(lutBuilder.io.done) {
        state := sStartRow
      }
    }

    is(sStartRow) {
      // Start prefetching first row into fill-side buffers
      weightStr.io.startRow := true.B
      weightStr.io.rowIdx := currentRow
      state := sWaitFill
    }

    is(sWaitFill) {
      when(weightStr.io.prefetchDone) {
        state := sSwapAndGo
      }
    }

    is(sSwapAndGo) {
      // Swap: make filled buffers the active ones
      weightStr.io.swap := true.B
      currentTile := 0.U
      state := sStartPrefetch
    }

    is(sStartPrefetch) {
      // Start prefetch for next row (if any)
      val nextRow = currentRow + 1.U
      when(nextRow < totalRows) {
        weightStr.io.startRow := true.B
        weightStr.io.rowIdx := nextRow
      }
      state := sLoadTile
    }

    is(sLoadTile) {
      // Present tile 0 address to weight BRAMs and LUT BRAMs (data valid next cycle)
      weightStr.io.tileIdx := 0.U
      computeCore.io.tileIdx := 0.U
      // Reset accumulator for new row
      computeCore.io.rowStart := true.B
      state := sConsumeWeight
    }

    is(sConsumeWeight) {
      // Weight data and LUT data are valid (addresses presented last cycle)
      weightStr.io.tileIdx := currentTile
      computeCore.io.tileIdx := currentTile

      // Feed weight data to compute core
      computeCore.io.nibbles   := weightStr.io.nibData
      computeCore.io.signs     := weightStr.io.signData
      computeCore.io.tileValid := true.B
      computeCore.io.tileIn    := true.B

      val nextTile = currentTile + 1.U
      when(nextTile >= tilesPerRow) {
        // All tiles for this row consumed
        pipelineFlush := 0.U
        state := sWaitPipeline
      }.otherwise {
        currentTile := nextTile
        // Present next tile address (data valid next cycle)
        weightStr.io.tileIdx := nextTile
        computeCore.io.tileIdx := nextTile
      }
    }

    is(sWaitPipeline) {
      // Wait for adder tree pipeline + accumulator to flush
      pipelineFlush := pipelineFlush + 1.U
      when(pipelineFlush >= totalPipeLatency) {
        state := sRowNext
      }
    }

    is(sRowNext) {
      val nextRow = currentRow + 1.U
      when(nextRow >= totalRows) {
        when(ddr3Mode) {
          state := sWriteResults
        }.otherwise {
          state := sDone
        }
      }.otherwise {
        currentRow := nextRow
        currentTile := 0.U
        when(weightStr.io.prefetchDone) {
          state := sSwapAndGo
        }.otherwise {
          state := sWaitFill
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
