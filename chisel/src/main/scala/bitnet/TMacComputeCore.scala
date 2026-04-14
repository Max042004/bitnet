package bitnet

import chisel3._
import chisel3.util._

/** T-MAC compute core: 32 LUT lookup engines → pipelined adder tree → accumulator.
  *
  * Each engine:
  *   1. Reads a 256-bit LUT word from its BRAM bank (1-cycle latency)
  *   2. MUXes out the 16-bit entry selected by the 4-bit nibble
  *   3. Applies sign correction: result = sign ? -value : +value (17-bit output)
  *
  * The 32 engine outputs feed a 5-level pipelined adder tree, producing
  * a 22-bit sum every cycle (after 5 cycles of pipeline latency).
  * The accumulator adds tree outputs across tiles for a complete row.
  */
class TMacComputeCore(implicit val cfg: TMacConfig) extends Module {
  val bankAddrW = log2Ceil(cfg.lutBankDepth)

  val io = IO(new Bundle {
    // Weight input (from weight streamer, 1 tile per cycle)
    val nibbles    = Input(UInt(cfg.avalonDataW.W))    // 128 bits = 32 × 4-bit nibbles
    val signs      = Input(UInt(cfg.numEngines.W))     // 32 sign bits
    val tileValid  = Input(Bool())                      // Weight data is valid this cycle

    // LUT BRAM interface (32 banks, read only during compute)
    val lutReadAddr = Output(Vec(cfg.numEngines, UInt(bankAddrW.W)))
    val lutReadData = Input(Vec(cfg.numEngines, UInt(cfg.lutWordW.W)))

    // Tile address for LUT BRAM (which tile we're processing)
    val tileIdx    = Input(UInt(cfg.dimW.W))

    // Configuration
    val numGroups  = Input(UInt(cfg.dimW.W))   // n3

    // Accumulator control
    val rowStart   = Input(Bool())   // Reset accumulator for new row
    val tileIn     = Input(Bool())   // A tile is being presented

    // Output
    val accumOut   = Output(SInt(cfg.accumW.W))
    val resultValid = Output(Bool())
  })

  val tilesPerRow = (io.numGroups + (cfg.numEngines - 1).U) / cfg.numEngines.U

  // --- Stage 1: Present LUT BRAM addresses ---
  // All engines read the same tile address (their bank has different data).
  // io.tileIdx is already the tile number, so using tileIdx >> log2(numEngines)
  // would alias 32 consecutive tiles onto the same LUT address.
  val tileAddr = io.tileIdx(bankAddrW - 1, 0)
  for (e <- 0 until cfg.numEngines) {
    io.lutReadAddr(e) := tileAddr
  }

  // --- Stage 2: Extract nibbles and signs, MUX LUT entries ---
  // (LUT data arrives 1 cycle after address was presented)
  // Pipeline the nibbles and signs to align with LUT data
  val nibblesD1 = RegNext(io.nibbles)
  val signsD1   = RegNext(io.signs)
  val tileValidD1 = RegNext(io.tileValid, false.B)

  // --- Stage 2a: 16:1 LUT MUX (registered to break timing) ---
  val lutEntryRegs = Reg(Vec(cfg.numEngines, SInt(cfg.lutWidth.W)))
  val signsD2 = RegNext(signsD1)
  val tileValidD2 = RegNext(tileValidD1, false.B)

  for (e <- 0 until cfg.numEngines) {
    val nibble = nibblesD1(e * cfg.nibbleW + cfg.nibbleW - 1, e * cfg.nibbleW)
    val lutWord = io.lutReadData(e)

    lutEntryRegs(e) := MuxLookup(nibble, 0.S)(
      (0 until cfg.lutEntries).map(i =>
        i.U -> lutWord(i * cfg.lutWidth + cfg.lutWidth - 1, i * cfg.lutWidth).asSInt
      )
    )
  }

  // --- Stage 2b: Sign correction on registered LUT entries ---
  val engineResults = Wire(Vec(cfg.numEngines, SInt(cfg.engineOutW.W)))
  for (e <- 0 until cfg.numEngines) {
    engineResults(e) := Mux(signsD2(e), -lutEntryRegs(e), lutEntryRegs(e))
  }

  // Pipeline register between engines and adder tree
  val engineRegs = Reg(Vec(cfg.numEngines, SInt(cfg.engineOutW.W)))
  val engineValidReg = RegInit(false.B)
  engineRegs := engineResults
  engineValidReg := tileValidD2

  // --- Adder tree: 32 inputs → 1 sum, 5 pipeline stages ---
  var current: Seq[SInt] = (0 until cfg.numEngines).map { i =>
    val w = Wire(SInt(cfg.treeOutW.W))
    w := engineRegs(i)
    w
  }
  var validPipe = engineValidReg

  for (_ <- 0 until cfg.treeDepth) {
    val half = current.length / 2
    val next = (0 until half).map { i =>
      current(2 * i) +& current(2 * i + 1)
    }
    current = next.map(x => RegNext(x))
    validPipe = RegNext(validPipe, false.B)
  }

  val treeSum = current.head
  val treeSumValid = validPipe

  // --- Accumulator ---
  val accumulator = RegInit(0.S(cfg.accumW.W))
  val accumReset  = RegInit(true.B)

  when(io.rowStart) {
    accumReset := true.B
  }

  when(treeSumValid) {
    val widened = Wire(SInt(cfg.accumW.W))
    widened := treeSum
    when(accumReset) {
      accumulator := widened
      accumReset := false.B
    }.otherwise {
      accumulator := accumulator + widened
    }
  }

  io.accumOut := accumulator

  // --- Track when accumulation is complete for this row ---
  val tileCount = RegInit(0.U(cfg.dimW.W))
  val tilesOutCount = RegInit(0.U(cfg.dimW.W))
  val accumulating = RegInit(false.B)

  when(io.rowStart) {
    tileCount := Mux(io.tileIn, 1.U, 0.U)
    tilesOutCount := 0.U
    accumulating := true.B
  }.elsewhen(io.tileIn) {
    tileCount := tileCount + 1.U
  }

  when(treeSumValid && accumulating) {
    tilesOutCount := tilesOutCount + 1.U
  }

  val accumDone = accumulating && (tilesOutCount === tilesPerRow) && (tilesPerRow > 0.U)

  when(accumDone) {
    accumulating := false.B
  }

  io.resultValid := accumDone
}
