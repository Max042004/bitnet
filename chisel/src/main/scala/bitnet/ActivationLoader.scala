package bitnet

import chisel3._
import chisel3.util._

/** Burst-reads INT8 activations from DDR3 into ActivationBuffer's banked BRAM.
  *
  * Pipelined sub-bursts: issues multiple bursts of maxBurstLen beats
  * back-to-back, starting the next burst as soon as the previous one's
  * read command is accepted. This keeps the DDR3 read pipeline full.
  *
  * Packing: avalonDataW bits / 8 bits = actsPerBeat INT8 activations per beat.
  * K=4096, 128-bit bus → 16 acts/beat → 256 beats total → 16 sub-bursts of 16.
  */
class ActivationLoader(implicit val cfg: BitNetConfig) extends Module {
  val actsPerBeat = (cfg.avalonDataW / cfg.activationW).max(1)
  val maxBurst = cfg.maxBurstLen

  val io = IO(new Bundle {
    val start    = Input(Bool())
    val ddr3Addr = Input(UInt(cfg.avalonAddrW.W))
    val dimK     = Input(UInt(cfg.dimW.W))
    val done     = Output(Bool())

    // Avalon-MM read master signals (directly wired, not bundled — muxed at top)
    val avalon_address       = Output(UInt(cfg.avalonAddrW.W))
    val avalon_read          = Output(Bool())
    val avalon_burstcount    = Output(UInt(cfg.burstCountW.W))
    val avalon_waitrequest   = Input(Bool())
    val avalon_readdatavalid = Input(Bool())
    val avalon_readdata      = Input(UInt(cfg.avalonDataW.W))

    // Bulk write to ActivationBuffer
    val bulkWriteEn   = Output(Bool())
    val bulkWriteData = Output(Vec(actsPerBeat, SInt(cfg.activationW.W)))
    val bulkWriteBase = Output(UInt(cfg.dimW.W))
  })

  val sIdle :: sBurst :: sDone :: Nil = Enum(3)
  val state = RegInit(sIdle)

  val bytesPerBeat = cfg.avalonDataW / 8
  val addr = RegInit(0.U(cfg.avalonAddrW.W))
  val totalBeats = RegInit(0.U(cfg.dimW.W))
  val beatsIssued = RegInit(0.U(cfg.dimW.W))
  val beatsReceived = RegInit(0.U(cfg.dimW.W))

  val remainingBeats = totalBeats - beatsIssued
  val thisBurstLen = Mux(remainingBeats > maxBurst.U, maxBurst.U, remainingBeats)

  // Defaults
  io.done := false.B
  io.avalon_address := addr
  io.avalon_read := false.B
  io.avalon_burstcount := thisBurstLen(cfg.burstCountW - 1, 0)
  io.bulkWriteEn := false.B
  io.bulkWriteBase := 0.U
  for (i <- 0 until actsPerBeat) {
    io.bulkWriteData(i) := 0.S
  }

  switch(state) {
    is(sIdle) {
      when(io.start) {
        val total = (io.dimK + (actsPerBeat - 1).U) >> log2Ceil(actsPerBeat).U
        totalBeats := total
        addr := io.ddr3Addr
        beatsIssued := 0.U
        beatsReceived := 0.U
        state := sBurst
      }
    }
    is(sBurst) {
      // Issue sub-bursts back-to-back
      when(beatsIssued < totalBeats) {
        io.avalon_read := true.B
        io.avalon_address := addr
        io.avalon_burstcount := thisBurstLen(cfg.burstCountW - 1, 0)
        when(!io.avalon_waitrequest) {
          beatsIssued := beatsIssued + thisBurstLen
          addr := addr + thisBurstLen * bytesPerBeat.U
        }
      }

      // Receive data concurrently
      when(io.avalon_readdatavalid) {
        io.bulkWriteEn := true.B
        io.bulkWriteBase := beatsReceived << log2Ceil(actsPerBeat).U
        for (i <- 0 until actsPerBeat) {
          io.bulkWriteData(i) := io.avalon_readdata(i * cfg.activationW + cfg.activationW - 1, i * cfg.activationW).asSInt
        }
        beatsReceived := beatsReceived + 1.U
      }

      // Done when all data received
      when(beatsReceived + io.avalon_readdatavalid.asUInt === totalBeats) {
        state := sDone
      }
    }
    is(sDone) {
      io.done := true.B
      state := sIdle
    }
  }
}
