package bitnet

import chisel3._
import chisel3.util._

/** Reads INT32 results from resultMem, packs into 256-bit words, burst-writes to DDR3.
  *
  * Packing: 256 bits / 32 bits = 8 INT32 results per beat.
  * M=1024: 1024 reads + 128 writes ≈ 23μs (was ~307μs via LW bridge).
  *
  * FSM: sIdle → sReadPack → sWrite → sDone
  */
class ResultWriter(implicit val cfg: BitNetConfig) extends Module {
  val resultsPerBeat = (cfg.avalonDataW / 32).max(1)  // 256/32 = 8 (min 1 for small test configs)

  val io = IO(new Bundle {
    val start    = Input(Bool())
    val ddr3Addr = Input(UInt(cfg.avalonAddrW.W))
    val dimM     = Input(UInt(cfg.dimW.W))
    val done     = Output(Bool())

    // Avalon-MM write master signals (directly wired, muxed at top)
    val avalon_address     = Output(UInt(cfg.avalonAddrW.W))
    val avalon_write       = Output(Bool())
    val avalon_writedata   = Output(UInt(cfg.avalonDataW.W))
    val avalon_byteenable  = Output(UInt((cfg.avalonDataW / 8).W))
    val avalon_waitrequest = Input(Bool())

    // ResultMem read port
    val resReadAddr = Output(UInt(cfg.dimW.W))
    val resReadData = Input(SInt(32.W))
  })

  val sIdle :: sReadPack :: sWrite :: sDone :: Nil = Enum(4)
  val state = RegInit(sIdle)

  val addr = RegInit(0.U(cfg.avalonAddrW.W))
  val resultIdx = RegInit(0.U(cfg.dimW.W))    // current result index being read
  val totalResults = RegInit(0.U(cfg.dimW.W))
  val packReg = RegInit(VecInit(Seq.fill(resultsPerBeat)(0.U(32.W))))
  val packIdx = RegInit(0.U(log2Ceil(resultsPerBeat + 1).W))  // 0..7
  val bytesPerBeat = cfg.avalonDataW / 8

  // SyncReadMem has 1-cycle latency: present addr cycle N, capture data cycle N+1
  val readPending = RegInit(false.B)

  // Defaults
  io.done := false.B
  io.avalon_address := addr
  io.avalon_write := false.B
  io.avalon_writedata := 0.U
  io.avalon_byteenable := 0.U
  io.resReadAddr := 0.U

  switch(state) {
    is(sIdle) {
      when(io.start) {
        addr := io.ddr3Addr
        resultIdx := 0.U
        totalResults := io.dimM
        packIdx := 0.U
        readPending := false.B
        state := sReadPack
      }
    }
    is(sReadPack) {
      when(!readPending) {
        // Issue read address
        io.resReadAddr := resultIdx
        readPending := true.B
      }.otherwise {
        // Capture data from previous cycle's read
        io.resReadAddr := resultIdx  // keep presenting for pipeline
        packReg(packIdx) := io.resReadData.asUInt
        val nextPackIdx = packIdx + 1.U
        val nextResultIdx = resultIdx + 1.U
        resultIdx := nextResultIdx
        readPending := false.B

        when(nextPackIdx === resultsPerBeat.U || nextResultIdx === totalResults) {
          // Pad remaining slots with zeros if partial
          when(nextPackIdx < resultsPerBeat.U) {
            for (i <- 0 until resultsPerBeat) {
              when(i.U >= nextPackIdx) {
                packReg(i) := 0.U
              }
            }
          }
          packIdx := nextPackIdx  // save for reference (not strictly needed)
          state := sWrite
        }.otherwise {
          packIdx := nextPackIdx
        }
      }
    }
    is(sWrite) {
      // Assemble 256-bit word from packReg
      io.avalon_write := true.B
      io.avalon_address := addr
      io.avalon_writedata := Cat(packReg.reverse)
      io.avalon_byteenable := ((BigInt(1) << bytesPerBeat) - 1).U

      when(!io.avalon_waitrequest) {
        addr := addr + bytesPerBeat.U
        packIdx := 0.U

        when(resultIdx >= totalResults) {
          state := sDone
        }.otherwise {
          readPending := false.B
          state := sReadPack
        }
      }
    }
    is(sDone) {
      io.done := true.B
      state := sIdle
    }
  }
}
