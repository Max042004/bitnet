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

/** Top-level BitNet accelerator integrating all sub-modules.
  *
  * Architecture:
  *   HPS ──Avalon-MM Slave──► ControlRegs ──► FSM
  *   DDR3 ◄──Avalon-MM Master── WeightStreamer / ActivationLoader / ResultWriter ◄── FSM
  *   ActivationBuffer ──► ComputeCore (Decoder→PEArray→AdderTree→Accum→Requant) ──► ResultBuffer
  *
  * Weight prefetch: double-buffered WeightStreamer allows overlapping DDR3
  * reads for row N+1 while row N is being computed, hiding DDR3 latency.
  *
  * DDR3 mode: When CTRL[1]=1, activations are loaded from DDR3 via ActivationLoader
  * before compute, and results are written to DDR3 via ResultWriter after compute.
  */
class BitNetAccelerator(implicit val cfg: BitNetConfig) extends Module {
  val io = IO(new Bundle {
    // Avalon-MM slave (HPS register access)
    val slave = new AvalonMMSlave()

    // Avalon-MM master (DDR3 reads + writes)
    val master = new AvalonMMMaster(cfg.avalonAddrW, cfg.avalonDataW, cfg.burstCountW)
  })

  // Sub-modules
  val controlRegs = Module(new ControlRegs)
  val actBuffer   = Module(new ActivationBuffer)
  val weightStr   = Module(new WeightStreamer)
  val computeCore = Module(new ComputeCore)
  val actLoader   = Module(new ActivationLoader)
  val resWriter   = Module(new ResultWriter)

  // Result buffer: stores raw accumulator values (full precision) for readback
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

  // ---- Result buffer read from control regs (default port) ----
  // Muxed with ResultWriter — see below
  val resReadAddr = Wire(UInt(cfg.dimW.W))
  val resReadData = resultMem.read(resReadAddr)
  controlRegs.io.resReadData := resReadData

  // ---- Main FSM ----
  val sIdle :: sLoadAct :: sStartRow :: sWaitFill :: sSwapAndGo :: sStartPrefetch :: sLoadTile :: sConsumeWeight :: sWaitPipeline :: sRowNext :: sWriteResults :: sDone :: Nil = Enum(12)
  val state = RegInit(sIdle)

  val currentRow = RegInit(0.U(cfg.dimW.W))
  val currentTile = RegInit(0.U(cfg.dimW.W))
  val tilesPerRow = RegInit(0.U(cfg.dimW.W))
  val totalRows = RegInit(0.U(cfg.dimW.W))
  val dimK = RegInit(0.U(cfg.dimW.W))
  val pipelineFlush = RegInit(0.U(4.W))
  val ddr3Mode = RegInit(false.B)

  val busy = state =/= sIdle
  val done = state === sDone

  controlRegs.io.busy := busy
  controlRegs.io.done := done
  controlRegs.io.perfCycles := perfCycles

  // ---- Weight streamer defaults ----
  weightStr.io.startRow := false.B
  weightStr.io.baseAddr := controlRegs.io.weightBase
  weightStr.io.dimK := controlRegs.io.dimK
  weightStr.io.rowIdx := currentRow
  weightStr.io.dequeue := false.B
  weightStr.io.swap := false.B

  // ---- ActivationLoader defaults ----
  actLoader.io.start := false.B
  actLoader.io.ddr3Addr := controlRegs.io.actDdr3Base
  actLoader.io.dimK := dimK

  // ---- ResultWriter defaults ----
  resWriter.io.start := false.B
  resWriter.io.ddr3Addr := controlRegs.io.resDdr3Base
  resWriter.io.dimM := totalRows

  // ResultWriter reads from resultMem
  resWriter.io.resReadData := resReadData

  // ResultMem read address mux: ResultWriter during sWriteResults, ControlRegs otherwise
  resReadAddr := Mux(state === sWriteResults, resWriter.io.resReadAddr, controlRegs.io.resReadAddr)

  // ---- Activation buffer tile address default ----
  actBuffer.io.tileOffset := currentTile << log2Ceil(cfg.numPEs).U

  // ---- Compute core connections ----
  computeCore.io.weightData := weightStr.io.weightData
  computeCore.io.weightValid := false.B
  computeCore.io.activations := actBuffer.io.activations
  computeCore.io.dimK := dimK
  computeCore.io.shiftAmt := controlRegs.io.shiftAmt
  computeCore.io.rowStart := false.B
  computeCore.io.tileIn := false.B

  // Write raw accumulator to result buffer (skip INT8 requantization for precision)
  val accumWide = Wire(SInt(32.W))
  accumWide := computeCore.io.accumOut
  when(computeCore.io.resultValid) {
    resultMem.write(resultWriteIdx, accumWide)
    resultWriteIdx := resultWriteIdx + 1.U
  }

  // Performance counter
  when(busy && !done) {
    perfCycles := perfCycles + 1.U
  }

  // ---- Master port mux ----
  // Time-multiplexed: sLoadAct → actLoader reads, compute → weightStr reads, sWriteResults → resWriter writes
  // Default: all deasserted
  io.master.address := 0.U
  io.master.read := false.B
  io.master.write := false.B
  io.master.writedata := 0.U
  io.master.byteenable := 0.U
  io.master.burstcount := 0.U

  // ActivationLoader Avalon signals
  actLoader.io.avalon_waitrequest := true.B
  actLoader.io.avalon_readdatavalid := false.B
  actLoader.io.avalon_readdata := 0.U

  // WeightStreamer Avalon signals
  weightStr.io.avalon.waitrequest := true.B
  weightStr.io.avalon.readdatavalid := false.B
  weightStr.io.avalon.readdata := 0.U

  // ResultWriter Avalon signals
  resWriter.io.avalon_waitrequest := true.B

  when(state === sLoadAct) {
    // ActivationLoader drives master reads
    io.master.address := actLoader.io.avalon_address
    io.master.read := actLoader.io.avalon_read
    io.master.burstcount := actLoader.io.avalon_burstcount
    actLoader.io.avalon_waitrequest := io.master.waitrequest
    actLoader.io.avalon_readdatavalid := io.master.readdatavalid
    actLoader.io.avalon_readdata := io.master.readdata
  }.elsewhen(state === sWriteResults) {
    // ResultWriter drives master writes
    io.master.address := resWriter.io.avalon_address
    io.master.write := resWriter.io.avalon_write
    io.master.writedata := resWriter.io.avalon_writedata
    io.master.byteenable := resWriter.io.avalon_byteenable
    io.master.burstcount := resWriter.io.avalon_burstcount
    resWriter.io.avalon_waitrequest := io.master.waitrequest
  }.otherwise {
    // WeightStreamer drives master reads (during compute states)
    io.master.address := weightStr.io.avalon.address
    io.master.read := weightStr.io.avalon.read
    io.master.burstcount := weightStr.io.avalon.burstcount
    weightStr.io.avalon.waitrequest := io.master.waitrequest
    weightStr.io.avalon.readdatavalid := io.master.readdatavalid
    weightStr.io.avalon.readdata := io.master.readdata
  }

  // ---- FSM ----
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
        ddr3Mode := controlRegs.io.ddr3Mode
        when(controlRegs.io.ddr3Mode) {
          state := sLoadAct
        }.otherwise {
          state := sStartRow
        }
      }
    }
    is(sLoadAct) {
      // Pulse start on entry (one cycle only)
      actLoader.io.start := !actLoader.io.done && (RegNext(state) =/= sLoadAct)
      when(actLoader.io.done) {
        state := sStartRow
      }
    }
    is(sStartRow) {
      // Fill the fill-side FIFO with the first row
      weightStr.io.startRow := true.B
      weightStr.io.rowIdx := currentRow
      state := sWaitFill
    }
    is(sWaitFill) {
      // Wait for the fill-side FIFO to finish loading
      when(weightStr.io.prefetchDone) {
        state := sSwapAndGo
      }
    }
    is(sSwapAndGo) {
      // Swap: make the filled FIFO the active one
      weightStr.io.swap := true.B
      currentTile := 0.U
      // Start prefetch on next cycle (after swap register updates)
      state := sStartPrefetch
    }
    is(sStartPrefetch) {
      // Now activeBuf has been updated, so startRow fills the correct FIFO
      val nextRow = currentRow + 1.U
      when(nextRow < totalRows) {
        weightStr.io.startRow := true.B
        weightStr.io.rowIdx := nextRow
      }
      state := sLoadTile
    }
    is(sLoadTile) {
      // Present tile 0 address to BRAM (data valid next cycle)
      actBuffer.io.tileOffset := 0.U
      // Reset accumulator for new row
      computeCore.io.rowStart := true.B
      state := sConsumeWeight
    }
    is(sConsumeWeight) {
      // BRAM output for currentTile is valid (address was presented last cycle)
      when(weightStr.io.dataReady) {
        weightStr.io.dequeue := true.B
        computeCore.io.weightValid := true.B
        computeCore.io.tileIn := true.B
        val nextTile = currentTile + 1.U
        when(nextTile >= tilesPerRow) {
          // All tiles for this row done, wait for pipeline
          pipelineFlush := 0.U
          state := sWaitPipeline
        }.otherwise {
          currentTile := nextTile
          // Prefetch next tile's activations (data valid next cycle)
          actBuffer.io.tileOffset := nextTile << log2Ceil(cfg.numPEs).U
          state := sConsumeWeight
        }
      }
    }
    is(sWaitPipeline) {
      // Wait for adder tree pipeline + requantize to flush
      pipelineFlush := pipelineFlush + 1.U
      when(pipelineFlush >= (cfg.treePipeStages + 3).U) {
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
        // Prefetch was already started in sSwapAndGo.
        // Check if it's done; if so, swap immediately, otherwise wait.
        when(weightStr.io.prefetchDone) {
          state := sSwapAndGo
        }.otherwise {
          state := sWaitFill
        }
      }
    }
    is(sWriteResults) {
      // Pulse start on entry (one cycle only)
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
