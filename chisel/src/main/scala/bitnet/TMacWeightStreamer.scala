package bitnet

import chisel3._
import chisel3.util._

/** Streams T-MAC weights (nibbles + signs) from DDR3 with double buffering.
  *
  * T-MAC weights are stored in two separate DDR3 arrays:
  *   Nibble array: 4 bits/group, 32 groups per 128-bit beat (= 1 tile/beat)
  *   Sign array:   1 bit/group, 128 groups per 128-bit beat
  *
  * For K=6912 (n3=2304, tilesPerRow=72):
  *   - 72 nibble beats + 18 sign beats = 90 beats per row
  *   - vs v1's 108 beats → 17% bandwidth reduction
  *
  * Double buffering: two BRAM pairs (nibBuf A/B, signBuf A/B) allow
  * prefetching row N+1 while computing row N. Swap toggles active pair.
  *
  * Compute side reads directly from active buffers:
  *   nibBuf[tileIdx] → 128-bit word → 32 nibbles
  *   signBuf[tileIdx/4] → 128-bit word → extract 32 signs at (tileIdx%4)*32
  */
class TMacWeightStreamer(implicit val cfg: TMacConfig) extends Module {
  val io = IO(new Bundle {
    // Control
    val startRow = Input(Bool())
    val nibBase  = Input(UInt(cfg.avalonAddrW.W))
    val signBase = Input(UInt(cfg.avalonAddrW.W))
    val dimK     = Input(UInt(cfg.dimW.W))
    val dimN3    = Input(UInt(cfg.dimW.W))
    val rowIdx   = Input(UInt(cfg.dimW.W))

    // Double-buffer control
    val swap         = Input(Bool())
    val prefetchDone = Output(Bool())

    // Avalon-MM master (bus width reads)
    val avalon = new AvalonMMReadMaster(cfg.avalonAddrW, cfg.avalonDataW, cfg.burstCountW)

    // Compute-side read interface
    val tileIdx  = Input(UInt(cfg.dimW.W))
    val nibData  = Output(UInt(cfg.avalonDataW.W))   // 128 bits = 32 nibbles
    val signData = Output(UInt(cfg.numEngines.W))     // 32 sign bits for this tile

    // Status
    val tilesPerRow = Output(UInt(cfg.dimW.W))
  })

  val busBytesPerBeat = cfg.avalonDataW / 8  // 16
  val maxBurst = cfg.maxBurstLen

  // --- Double-buffered BRAMs ---
  val nibBufA  = SyncReadMem(cfg.maxTilesPerRow, UInt(cfg.avalonDataW.W))
  val nibBufB  = SyncReadMem(cfg.maxTilesPerRow, UInt(cfg.avalonDataW.W))
  val signBufA = SyncReadMem(cfg.maxSignBeatsPerRow, UInt(cfg.avalonDataW.W))
  val signBufB = SyncReadMem(cfg.maxSignBeatsPerRow, UInt(cfg.avalonDataW.W))

  // activeBuf: false = A is active (compute reads), B is fill (DDR3 writes)
  //            true  = B is active (compute reads), A is fill (DDR3 writes)
  val activeBuf = RegInit(false.B)

  when(io.swap) {
    activeBuf := !activeBuf
  }

  // --- Prefetch FSM (fills inactive buffer pair) ---
  val sIdle :: sNibBurst :: sSignBurst :: sFillDone :: Nil = Enum(4)
  val state = RegInit(sIdle)

  val addr = RegInit(0.U(cfg.avalonAddrW.W))
  val totalNibBeats = RegInit(0.U(cfg.dimW.W))
  val totalSignBeats = RegInit(0.U(cfg.dimW.W))
  val beatsIssued = RegInit(0.U(cfg.dimW.W))
  val beatsReceived = RegInit(0.U(cfg.dimW.W))
  val tilesPerRowReg = RegInit(0.U(cfg.dimW.W))

  val fillDoneReg = RegInit(false.B)
  io.prefetchDone := fillDoneReg

  when(io.swap) {
    fillDoneReg := false.B
  }

  io.tilesPerRow := tilesPerRowReg

  // n3 = dimK / 3 is supplied directly by the HPS via a dedicated control
  // register, eliminating a 17-level combinational divider that previously
  // dominated the critical path at 100 MHz.

  // Burst length calculation
  val totalBeats = Mux(state === sNibBurst, totalNibBeats, totalSignBeats)
  val remainingBeats = totalBeats - beatsIssued
  val thisBurstLen = Mux(remainingBeats > maxBurst.U, maxBurst.U, remainingBeats)

  // Avalon defaults
  io.avalon.address    := addr
  io.avalon.read       := false.B
  io.avalon.burstcount := thisBurstLen(cfg.burstCountW - 1, 0)

  // Write incoming data to fill-side buffers
  val nibWriteIdx = RegInit(0.U(cfg.dimW.W))
  val signWriteIdx = RegInit(0.U(cfg.dimW.W))

  // Per-GEMV row strides (bytes). Computed once on row 0 and reused thereafter
  // so that subsequent rows accumulate (base += stride) instead of recomputing
  // rowIdx * stride, eliminating two variable×variable LUT multipliers.
  val nibRowStride  = RegInit(0.U(cfg.avalonAddrW.W))
  val signRowStride = RegInit(0.U(cfg.avalonAddrW.W))
  val nibRowBase    = RegInit(0.U(cfg.avalonAddrW.W))
  val signRowBase   = RegInit(0.U(cfg.avalonAddrW.W))

  switch(state) {
    is(sIdle) {
      when(io.startRow) {
        fillDoneReg := false.B
        // n3 supplied by HPS (no on-chip divider)
        val n3 = io.dimN3
        val tpr = (n3 + (cfg.numEngines - 1).U) / cfg.numEngines.U
        val sbr = (n3 + (cfg.signsPerBeat - 1).U) / cfg.signsPerBeat.U
        tilesPerRowReg := tpr
        totalNibBeats := tpr
        totalSignBeats := sbr

        // tpr/sbr × 16 → left-shift-by-4 (busBytesPerBeat = 16 is const).
        val nibStride  = (tpr << 4).asUInt
        val signStride = (sbr << 4).asUInt

        // On row 0: initialize bases to the array bases. On row N>0: advance
        // the previous base by one stride. No variable×variable multiplies.
        val newNibBase  = Mux(io.rowIdx === 0.U, io.nibBase,  nibRowBase  + nibRowStride)
        val newSignBase = Mux(io.rowIdx === 0.U, io.signBase, signRowBase + signRowStride)
        nibRowStride  := nibStride
        signRowStride := signStride
        nibRowBase    := newNibBase
        signRowBase   := newSignBase

        // Start nibble burst from this row's nibble base
        addr := newNibBase
        beatsIssued := 0.U
        beatsReceived := 0.U
        nibWriteIdx := 0.U
        state := sNibBurst
      }
    }
    is(sNibBurst) {
      // Issue sub-bursts for nibble data
      when(beatsIssued < totalNibBeats) {
        io.avalon.read := true.B
        when(!io.avalon.waitrequest) {
          beatsIssued := beatsIssued + thisBurstLen
          addr := addr + thisBurstLen * busBytesPerBeat.U
        }
      }

      // Receive nibble data and write to fill-side nibBuf
      when(io.avalon.readdatavalid) {
        when(!activeBuf) {
          nibBufB.write(nibWriteIdx, io.avalon.readdata)
        }.otherwise {
          nibBufA.write(nibWriteIdx, io.avalon.readdata)
        }
        nibWriteIdx := nibWriteIdx + 1.U
        beatsReceived := beatsReceived + 1.U
      }

      // Transition to sign burst when all nibble data received
      when(beatsReceived + io.avalon.readdatavalid.asUInt === totalNibBeats) {
        // Use the per-row signRowBase that was latched in sIdle. No multiply.
        addr := signRowBase
        beatsIssued := 0.U
        beatsReceived := 0.U
        signWriteIdx := 0.U
        state := sSignBurst
      }
    }
    is(sSignBurst) {
      // Issue sub-bursts for sign data
      when(beatsIssued < totalSignBeats) {
        io.avalon.read := true.B
        when(!io.avalon.waitrequest) {
          beatsIssued := beatsIssued + thisBurstLen
          addr := addr + thisBurstLen * busBytesPerBeat.U
        }
      }

      // Receive sign data and write to fill-side signBuf
      when(io.avalon.readdatavalid) {
        when(!activeBuf) {
          signBufB.write(signWriteIdx, io.avalon.readdata)
        }.otherwise {
          signBufA.write(signWriteIdx, io.avalon.readdata)
        }
        signWriteIdx := signWriteIdx + 1.U
        beatsReceived := beatsReceived + 1.U
      }

      // Done when all sign data received
      when(beatsReceived + io.avalon.readdatavalid.asUInt === totalSignBeats) {
        fillDoneReg := true.B
        state := sIdle
      }
    }
    is(sFillDone) {
      fillDoneReg := true.B
      state := sIdle
    }
  }

  when(io.startRow && state === sIdle) {
    fillDoneReg := false.B
  }

  // --- Compute-side read from active buffers ---
  // Nibble read: nibBuf[tileIdx] → 128-bit word
  val nibReadData = Mux(activeBuf,
    nibBufB.read(io.tileIdx),
    nibBufA.read(io.tileIdx)
  )
  io.nibData := nibReadData

  // Sign read: signBuf[tileIdx / 4] → 128-bit word, extract 32 bits at (tileIdx % 4) * 32
  val signBeatIdx = io.tileIdx >> 2.U
  val signOffset = io.tileIdx(1, 0)  // tileIdx % 4

  val signReadData = Mux(activeBuf,
    signBufB.read(signBeatIdx),
    signBufA.read(signBeatIdx)
  )

  // Extract 32 sign bits from the correct position (registered to match BRAM latency)
  val signOffsetD1 = RegNext(signOffset)
  val signWord = Wire(UInt(cfg.numEngines.W))
  signWord := MuxLookup(signOffsetD1, 0.U)(Seq(
    0.U -> signReadData(31, 0),
    1.U -> signReadData(63, 32),
    2.U -> signReadData(95, 64),
    3.U -> signReadData(127, 96)
  ))
  io.signData := signWord
}
