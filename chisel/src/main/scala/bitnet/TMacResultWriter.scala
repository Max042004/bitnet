package bitnet

import chisel3._
import chisel3.util._

/** Reads INT32 results from resultMem, packs into bus-width words, burst-writes to DDR3.
  *
  * Adapted from ResultWriter for TMacConfig. Packs avalonDataW/32 INT32 results
  * per beat and uses burst writes for DDR3 efficiency.
  */
class TMacResultWriter(implicit val cfg: TMacConfig) extends Module {
  val resultsPerBeat = (cfg.avalonDataW / 32).max(1)  // 4
  val bytesPerBeat = cfg.avalonDataW / 8

  val io = IO(new Bundle {
    val start    = Input(Bool())
    val ddr3Addr = Input(UInt(cfg.avalonAddrW.W))
    val dimM     = Input(UInt(cfg.dimW.W))
    val done     = Output(Bool())

    // Avalon-MM write master signals (muxed at top)
    val avalon_address     = Output(UInt(cfg.avalonAddrW.W))
    val avalon_write       = Output(Bool())
    val avalon_writedata   = Output(UInt(cfg.avalonDataW.W))
    val avalon_byteenable  = Output(UInt((cfg.avalonDataW / 8).W))
    val avalon_waitrequest = Input(Bool())
    val avalon_burstcount  = Output(UInt(cfg.burstCountW.W))

    // ResultMem read port
    val resReadAddr = Output(UInt(cfg.dimW.W))
    val resReadData = Input(SInt(32.W))
  })

  val maxBeatsPerBurst = cfg.maxBurstLen
  val burstBuf = Reg(Vec(maxBeatsPerBurst, UInt(cfg.avalonDataW.W)))

  val sIdle :: sReadPack :: sBurstWrite :: sDone :: Nil = Enum(4)
  val state = RegInit(sIdle)

  val addr = RegInit(0.U(cfg.avalonAddrW.W))
  val resultIdx = RegInit(0.U(cfg.dimW.W))
  val totalResults = RegInit(0.U(cfg.dimW.W))
  val packReg = RegInit(VecInit(Seq.fill(resultsPerBeat)(0.U(32.W))))
  val packIdx = RegInit(0.U(log2Ceil(resultsPerBeat + 1).W))
  val burstIdx = RegInit(0.U(log2Ceil(maxBeatsPerBurst + 1).W))
  val burstWriteIdx = RegInit(0.U(log2Ceil(maxBeatsPerBurst + 1).W))
  val currentBurstLen = RegInit(0.U(cfg.burstCountW.W))
  val readPending = RegInit(false.B)

  // Defaults
  io.done := false.B
  io.avalon_address := addr
  io.avalon_write := false.B
  io.avalon_writedata := 0.U
  io.avalon_byteenable := 0.U
  io.avalon_burstcount := currentBurstLen
  io.resReadAddr := 0.U

  switch(state) {
    is(sIdle) {
      when(io.start) {
        addr := io.ddr3Addr
        resultIdx := 0.U
        totalResults := io.dimM
        packIdx := 0.U
        burstIdx := 0.U
        readPending := false.B
        state := sReadPack
      }
    }
    is(sReadPack) {
      when(!readPending) {
        io.resReadAddr := resultIdx
        readPending := true.B
      }.otherwise {
        io.resReadAddr := resultIdx
        packReg(packIdx) := io.resReadData.asUInt
        val nextPackIdx = packIdx + 1.U
        val nextResultIdx = resultIdx + 1.U
        resultIdx := nextResultIdx
        readPending := false.B

        when(nextPackIdx === resultsPerBeat.U || nextResultIdx === totalResults) {
          val beatWord = Wire(UInt(cfg.avalonDataW.W))
          val slots = Wire(Vec(resultsPerBeat, UInt(32.W)))
          for (i <- 0 until resultsPerBeat) {
            when(i.U === packIdx) {
              slots(i) := io.resReadData.asUInt
            }.elsewhen(i.U < nextPackIdx) {
              slots(i) := packReg(i)
            }.otherwise {
              slots(i) := 0.U
            }
          }
          beatWord := Cat(slots.reverse)
          burstBuf(burstIdx) := beatWord

          val nextBurstIdx = burstIdx + 1.U
          burstIdx := nextBurstIdx
          packIdx := 0.U

          when(nextBurstIdx === maxBeatsPerBurst.U || nextResultIdx === totalResults) {
            currentBurstLen := nextBurstIdx.pad(cfg.burstCountW)
            burstWriteIdx := 0.U
            state := sBurstWrite
          }
        }.otherwise {
          packIdx := nextPackIdx
        }
      }
    }
    is(sBurstWrite) {
      io.avalon_write := true.B
      io.avalon_address := addr
      io.avalon_burstcount := currentBurstLen
      io.avalon_writedata := burstBuf(burstWriteIdx)
      io.avalon_byteenable := ((BigInt(1) << bytesPerBeat) - 1).U

      when(!io.avalon_waitrequest) {
        val nextWriteIdx = burstWriteIdx + 1.U
        burstWriteIdx := nextWriteIdx

        when(nextWriteIdx === currentBurstLen) {
          addr := addr + currentBurstLen * bytesPerBeat.U
          burstIdx := 0.U

          when(resultIdx >= totalResults) {
            state := sDone
          }.otherwise {
            readPending := false.B
            state := sReadPack
          }
        }
      }
    }
    is(sDone) {
      io.done := true.B
      state := sIdle
    }
  }
}
