package bitnet

import chisel3._
import chisel3.util._

/** Avalon-MM slave register interface for T-MAC accelerator HPS control.
  *
  * Register map:
  *   0x00  CTRL            W    [0]=START (auto-clear), [1]=DDR3_MODE
  *   0x04  STATUS          R    [0]=BUSY, [1]=DONE
  *   0x08  NIB_BASE        R/W  DDR3 base address for T-MAC nibble array
  *   0x0C  DIM_M           R/W  Output dimension
  *   0x10  DIM_K           R/W  Input dimension (must be divisible by 3)
  *   0x14  SIGN_BASE       R/W  DDR3 base address for T-MAC sign array
  *   0x18  PERF_CYCLES     R    Performance counter
  *   0x28  ACT_DDR3_BASE   R/W  DDR3 byte address for activations
  *   0x2C  RES_DDR3_BASE   R/W  DDR3 byte address for results
  *   0x80+  ACT_DATA       W    Activation buffer write (stride 4)
  *   0x8000+ RES_DATA      R    Result buffer read (stride 4, INT32)
  */
class TMacControlRegs(implicit val cfg: TMacConfig) extends Module {
  val io = IO(new Bundle {
    val avalon = new AvalonMMSlave()

    // Control outputs
    val start      = Output(Bool())
    val busy       = Input(Bool())
    val done       = Input(Bool())
    val nibBase    = Output(UInt(cfg.avalonAddrW.W))
    val signBase   = Output(UInt(cfg.avalonAddrW.W))
    val dimM       = Output(UInt(cfg.dimW.W))
    val dimK       = Output(UInt(cfg.dimW.W))
    val perfCycles = Input(UInt(32.W))

    // Activation buffer write port
    val actWriteEn   = Output(Bool())
    val actWriteAddr = Output(UInt(cfg.dimW.W))
    val actWriteData = Output(SInt(cfg.activationW.W))

    // Result buffer read port (raw accumulator, 32-bit)
    val resReadAddr = Output(UInt(cfg.dimW.W))
    val resReadData = Input(SInt(32.W))

    // DDR3 mode
    val actDdr3Base = Output(UInt(cfg.avalonAddrW.W))
    val resDdr3Base = Output(UInt(cfg.avalonAddrW.W))
    val ddr3Mode    = Output(Bool())
  })

  val regNibBase     = RegInit(0.U(cfg.avalonAddrW.W))
  val regSignBase    = RegInit(0.U(cfg.avalonAddrW.W))
  val regDimM        = RegInit(0.U(cfg.dimW.W))
  val regDimK        = RegInit(0.U(cfg.dimW.W))
  val regStart       = WireDefault(false.B)
  val statusDone     = RegInit(false.B)
  val regActDdr3Base = RegInit(0.U(cfg.avalonAddrW.W))
  val regResDdr3Base = RegInit(0.U(cfg.avalonAddrW.W))
  val regDdr3Mode    = WireDefault(false.B)

  when(io.done) {
    statusDone := true.B
  }

  // Outputs
  io.start       := regStart
  io.nibBase     := regNibBase
  io.signBase    := regSignBase
  io.dimM        := regDimM
  io.dimK        := regDimK
  io.actDdr3Base := regActDdr3Base
  io.resDdr3Base := regResDdr3Base
  io.ddr3Mode    := regDdr3Mode

  // Activation write defaults
  io.actWriteEn   := false.B
  io.actWriteAddr := 0.U
  io.actWriteData := 0.S

  // Result read default
  io.resReadAddr := 0.U

  val addr = io.avalon.address

  // Write logic
  when(io.avalon.write) {
    switch(addr) {
      is(0x00.U) {
        regStart := io.avalon.writedata(0)
        when(io.avalon.writedata(0)) {
          statusDone := false.B
          regDdr3Mode := io.avalon.writedata(1)
        }
      }
      is(0x08.U) { regNibBase := io.avalon.writedata }
      is(0x0C.U) { regDimM := io.avalon.writedata }
      is(0x10.U) { regDimK := io.avalon.writedata }
      is(0x14.U) { regSignBase := io.avalon.writedata }
      is(0x28.U) { regActDdr3Base := io.avalon.writedata }
      is(0x2C.U) { regResDdr3Base := io.avalon.writedata }
    }

    // Activation data write: addresses 0x80 to 0x80 + (maxDimK-1)*4
    when(addr >= 0x80.U && addr < (0x80 + cfg.maxDimK * 4).U) {
      io.actWriteEn   := true.B
      io.actWriteAddr := (addr - 0x80.U) >> 2
      io.actWriteData := io.avalon.writedata(cfg.activationW - 1, 0).asSInt
    }
  }

  // Read logic (readLatency = 1: registered outputs)
  val regReadData = RegInit(0.U(32.W))
  val readIsResult = RegNext(io.avalon.read && addr >= 0x8000.U, false.B)

  when(io.avalon.read) {
    switch(addr) {
      is(0x04.U) { regReadData := Cat(0.U(30.W), statusDone, io.busy) }
      is(0x08.U) { regReadData := regNibBase }
      is(0x0C.U) { regReadData := regDimM }
      is(0x10.U) { regReadData := regDimK }
      is(0x14.U) { regReadData := regSignBase }
      is(0x18.U) { regReadData := io.perfCycles }
      is(0x28.U) { regReadData := regActDdr3Base }
      is(0x2C.U) { regReadData := regResDdr3Base }
    }

    // Result buffer read
    when(addr >= 0x8000.U) {
      io.resReadAddr := (addr - 0x8000.U) >> 2
    }
  }

  io.avalon.readdata := Mux(readIsResult, io.resReadData.asUInt, regReadData)
}
