package bitnet

import chisel3._

/** BRAM-backed accumulator array for tile-major T-MAC computation.
  *
  * Stores M partial-sum accumulators in SyncReadMem. Supports read-modify-write:
  * cycle N: present readAddr → cycle N+1: readData available, add treeOutput, write back.
  *
  * The caller must track row indices through the adder tree pipeline and present
  * them aligned with the tree output (addAddr/addData arrive together).
  */
class AccumulatorArray(implicit val cfg: BitNetConfig) extends Module {
  val io = IO(new Bundle {
    // Read-modify-write port (add treeOutput to accum[addr])
    val addEn   = Input(Bool())
    val addAddr = Input(UInt(cfg.dimW.W))
    val addData = Input(SInt(cfg.accumW.W))

    // Direct read port (for result readback)
    val readAddr = Input(UInt(cfg.dimW.W))
    val readData = Output(SInt(cfg.accumW.W))

    // Clear control
    val clearEn   = Input(Bool())
    val clearAddr = Input(UInt(cfg.dimW.W))
  })

  val mem = SyncReadMem(cfg.maxDimM, SInt(cfg.accumW.W))

  // Read-modify-write pipeline:
  // Stage 0: present read address
  // Stage 1: read data available, compute sum, write back
  val rmwAddr1  = RegNext(io.addAddr)
  val rmwEn1    = RegNext(io.addEn, false.B)
  val rmwData1  = RegNext(io.addData)

  // Read for RMW (1-cycle latency)
  val rmwReadData = mem.read(io.addAddr, io.addEn)

  // Write back: old value + new data
  when(rmwEn1) {
    mem.write(rmwAddr1, rmwReadData + rmwData1)
  }

  // Clear: write zero
  when(io.clearEn) {
    mem.write(io.clearAddr, 0.S)
  }

  // Direct read port for result readback (1-cycle latency)
  io.readData := mem.read(io.readAddr)
}
