package bitnet

import chisel3._
import chisel3.util._

/** Burst-reads INT8 activations from DDR3 into ActivationBuffer's banked BRAM.
  *
  * Packing: 256 bits / 8 bits = 32 INT8 activations per beat.
  * K=4096 → 128 beats across multiple bursts (max burst = 32 beats).
  *
  * FSM: sIdle → sRead → sFill → (loop if more) → sDone
  *
  * burstCountW=6 limits a single burst to 63 beats. We use maxBurstBeats=32
  * (matching WeightStreamer) and issue multiple bursts to cover all K activations.
  */
class ActivationLoader(implicit val cfg: BitNetConfig) extends Module {
  val actsPerBeat = (cfg.avalonDataW / cfg.activationW).max(1)  // 256/8 = 32 (min 1 for small test configs)
  val maxBurstBeats = 1 << (cfg.burstCountW - 1)  // 32 for burstCountW=6

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

  val sIdle :: sRead :: sFill :: sDone :: Nil = Enum(4)
  val state = RegInit(sIdle)

  val bytesPerBeat = cfg.avalonDataW / 8
  val addr = RegInit(0.U(cfg.avalonAddrW.W))
  val totalBeats = RegInit(0.U(cfg.dimW.W))       // total beats remaining across all bursts
  val currentBurst = RegInit(0.U(cfg.burstCountW.W))  // beats in current burst
  val burstBeatsLeft = RegInit(0.U(cfg.burstCountW.W)) // beats left in current burst
  val beatsReceived = RegInit(0.U(cfg.dimW.W))     // global beat counter (for bulkWriteBase)

  // Defaults
  io.done := false.B
  io.avalon_address := addr
  io.avalon_read := false.B
  io.avalon_burstcount := currentBurst
  io.bulkWriteEn := false.B
  io.bulkWriteBase := 0.U
  for (i <- 0 until actsPerBeat) {
    io.bulkWriteData(i) := 0.S
  }

  switch(state) {
    is(sIdle) {
      when(io.start) {
        // ceil(K / actsPerBeat)
        val total = (io.dimK + (actsPerBeat - 1).U) >> log2Ceil(actsPerBeat).U
        totalBeats := total
        addr := io.ddr3Addr
        beatsReceived := 0.U
        state := sRead
      }
    }
    is(sRead) {
      // Issue burst of min(totalBeats, maxBurstBeats)
      val burst = Mux(totalBeats > maxBurstBeats.U, maxBurstBeats.U, totalBeats)(cfg.burstCountW - 1, 0)
      currentBurst := burst
      burstBeatsLeft := burst
      io.avalon_read := true.B
      io.avalon_address := addr
      io.avalon_burstcount := burst
      when(!io.avalon_waitrequest) {
        state := sFill
      }
    }
    is(sFill) {
      when(io.avalon_readdatavalid) {
        // Unpack activations from readdata
        io.bulkWriteEn := true.B
        io.bulkWriteBase := beatsReceived << log2Ceil(actsPerBeat).U
        for (i <- 0 until actsPerBeat) {
          io.bulkWriteData(i) := io.avalon_readdata(i * cfg.activationW + cfg.activationW - 1, i * cfg.activationW).asSInt
        }
        beatsReceived := beatsReceived + 1.U
        val nextLeft = burstBeatsLeft - 1.U
        burstBeatsLeft := nextLeft
        when(nextLeft === 0.U) {
          // Current burst done — advance address and totalBeats
          val used = currentBurst
          totalBeats := totalBeats - used
          addr := addr + (used * bytesPerBeat.U)
          when(totalBeats - used === 0.U) {
            state := sDone
          }.otherwise {
            state := sRead  // issue next burst
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
