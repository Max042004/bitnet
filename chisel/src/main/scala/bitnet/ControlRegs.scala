package bitnet

import chisel3._
import chisel3.util._

/** Avalon-MM slave register interface for HPS control.
  *
  * Register map:
  *   0x00  CTRL         W    [0]=START (auto-clear)
  *   0x04  STATUS       R    [0]=BUSY, [1]=DONE
  *   0x08  WEIGHT_BASE  R/W  DDR3 base address for weights
  *   0x0C  DIM_M        R/W  Output dimension
  *   0x10  DIM_K        R/W  Input/reduction dimension
  *   0x14  SHIFT_AMT    R/W  Requantization right-shift
  *   0x18  PERF_CYCLES  R    Performance counter
  *   0x80+  ACT_DATA    W    Activation buffer write (byte-addressed, stride 4, up to maxDimK entries)
  *   0x4000+ RES_DATA   R    Result buffer read (byte-addressed, stride 4)
  */
class AvalonMMSlave(addrW: Int = 15, dataW: Int = 32) extends Bundle {
  val address   = Input(UInt(addrW.W))
  val read      = Input(Bool())
  val write     = Input(Bool())
  val writedata = Input(UInt(dataW.W))
  val readdata  = Output(UInt(dataW.W))
}

class ControlRegs(implicit val cfg: BitNetConfig) extends Module {
  val io = IO(new Bundle {
    val avalon = new AvalonMMSlave()

    // Control outputs
    val start     = Output(Bool())
    val busy      = Input(Bool())
    val done      = Input(Bool())
    val weightBase = Output(UInt(cfg.avalonAddrW.W))
    val dimM      = Output(UInt(cfg.dimW.W))
    val dimK      = Output(UInt(cfg.dimW.W))
    val shiftAmt  = Output(UInt(5.W))
    val perfCycles = Input(UInt(32.W))

    // Activation buffer write port
    val actWriteEn   = Output(Bool())
    val actWriteAddr = Output(UInt(cfg.dimW.W))
    val actWriteData = Output(SInt(cfg.activationW.W))

    // Result buffer read port (raw accumulator, 32-bit)
    val resReadAddr = Output(UInt(cfg.dimW.W))
    val resReadData = Input(SInt(32.W))
  })

  val regWeightBase = RegInit(0.U(cfg.avalonAddrW.W))
  val regDimM       = RegInit(0.U(cfg.dimW.W))
  val regDimK       = RegInit(0.U(cfg.dimW.W))
  val regShiftAmt   = RegInit(0.U(5.W))
  val regStart      = WireDefault(false.B)
  val statusDone    = RegInit(false.B)

  when(io.done) {
    statusDone := true.B
  }

  // Outputs
  io.start     := regStart
  io.weightBase := regWeightBase
  io.dimM      := regDimM
  io.dimK      := regDimK
  io.shiftAmt  := regShiftAmt

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
        // Clear done on start
        when(io.avalon.writedata(0)) {
          statusDone := false.B
        }
      }
      is(0x08.U) { regWeightBase := io.avalon.writedata }
      is(0x0C.U) { regDimM := io.avalon.writedata }
      is(0x10.U) { regDimK := io.avalon.writedata }
      is(0x14.U) { regShiftAmt := io.avalon.writedata(4, 0) }
    }

    // Activation data write: addresses 0x80 to 0x80 + (maxDimK-1)*4
    when(addr >= 0x80.U && addr < (0x80 + cfg.maxDimK * 4).U) {
      io.actWriteEn   := true.B
      io.actWriteAddr := (addr - 0x80.U) >> 2
      io.actWriteData := io.avalon.writedata(cfg.activationW - 1, 0).asSInt
    }
  }

  // Read logic (readLatency = 1: register outputs, BRAM has natural 1-cycle latency)
  val regReadData = RegInit(0.U(32.W))
  val readIsResult = RegNext(io.avalon.read && addr >= 0x4000.U, false.B)

  when(io.avalon.read) {
    // Combinational register reads — registered for readLatency=1
    switch(addr) {
      is(0x04.U) { regReadData := Cat(0.U(30.W), statusDone, io.busy) }
      is(0x08.U) { regReadData := regWeightBase }
      is(0x0C.U) { regReadData := regDimM }
      is(0x10.U) { regReadData := regDimK }
      is(0x14.U) { regReadData := regShiftAmt }
      is(0x18.U) { regReadData := io.perfCycles }
    }

    // Result buffer read: present address to SyncReadMem (output arrives next cycle)
    when(addr >= 0x4000.U) {
      io.resReadAddr := (addr - 0x4000.U) >> 2
    }
  }

  // Output mux: BRAM data for result reads, registered data for register reads
  io.avalon.readdata := Mux(readIsResult, io.resReadData.asUInt, regReadData)
}
