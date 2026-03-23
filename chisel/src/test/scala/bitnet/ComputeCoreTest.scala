package bitnet

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ComputeCoreTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val cfg: BitNetConfig = BitNetConfig(numPEs = 8, avalonDataW = 16, maxDimK = 8, maxDimM = 4)

  behavior of "ComputeCore (T-MAC)"

  /** Pack weights: 0→00, +1→01, -1→10 */
  def packWeights(weights: Seq[Int]): BigInt = {
    var packed = BigInt(0)
    for (i <- weights.indices) {
      val enc = weights(i) match { case 0 => 0; case 1 => 1; case -1 => 2 }
      packed = packed | (BigInt(enc) << (i * 2))
    }
    packed
  }

  it should "build LUTs and compute single-tile dot product for one row" in {
    test(new ComputeCore) { dut =>
      // numPEs=8, numGroups=2, groupSize=4
      // Activations: [1, 1, 1, 1, 1, 1, 1, 1]
      // Weights: all +1
      // Expected dot product: 8
      for (i <- 0 until cfg.numPEs) {
        dut.io.activations(i).poke(1.S)
      }

      // Clear accumulator for row 0
      dut.io.accumClearEn.poke(true.B)
      dut.io.accumClearAddr.poke(0.U)
      dut.clock.step(1)
      dut.io.accumClearEn.poke(false.B)

      // Build LUTs
      dut.io.lutBuildStart.poke(true.B)
      dut.clock.step(1)
      dut.io.lutBuildStart.poke(false.B)

      // Wait for LUT build to complete
      while (!dut.io.lutBuildDone.peek().litToBoolean) {
        dut.clock.step(1)
      }

      // Feed weight for row 0
      val packed = packWeights(Seq.fill(cfg.numPEs)(1))
      dut.io.weightData.poke(packed.U)
      dut.io.weightValid.poke(true.B)
      dut.io.rowIdx.poke(0.U)
      dut.clock.step(1)
      dut.io.weightValid.poke(false.B)

      // Wait for pipeline: 1 (indexer reg) + tmacTreePipeStages (tree) + 2 (accum RMW)
      dut.clock.step(cfg.tmacTreePipeStages + 4)

      // Read accumulator for row 0
      dut.io.accumReadAddr.poke(0.U)
      dut.clock.step(1) // SyncReadMem 1-cycle latency
      val accumVal = dut.io.accumReadData.peek().litValue.toInt
      println(s"Accumulator[0] = $accumVal (expected: 8)")
      assert(accumVal == 8, s"Expected 8, got $accumVal")
    }
  }

  it should "compute dot product with mixed weights" in {
    test(new ComputeCore) { dut =>
      // Activations: [10, 10, 10, 10, 10, 10, 10, 10]
      // Weights: first 4 = +1, last 4 = -1
      // Expected: 4*10 - 4*10 = 0
      for (i <- 0 until cfg.numPEs) {
        dut.io.activations(i).poke(10.S)
      }

      dut.io.accumClearEn.poke(true.B)
      dut.io.accumClearAddr.poke(0.U)
      dut.clock.step(1)
      dut.io.accumClearEn.poke(false.B)

      dut.io.lutBuildStart.poke(true.B)
      dut.clock.step(1)
      dut.io.lutBuildStart.poke(false.B)

      while (!dut.io.lutBuildDone.peek().litToBoolean) {
        dut.clock.step(1)
      }

      val weights = Seq(1, 1, 1, 1, -1, -1, -1, -1)
      dut.io.weightData.poke(packWeights(weights).U)
      dut.io.weightValid.poke(true.B)
      dut.io.rowIdx.poke(0.U)
      dut.clock.step(1)
      dut.io.weightValid.poke(false.B)

      dut.clock.step(cfg.tmacTreePipeStages + 4)

      dut.io.accumReadAddr.poke(0.U)
      dut.clock.step(1)
      val accumVal = dut.io.accumReadData.peek().litValue.toInt
      println(s"Accumulator[0] = $accumVal (expected: 0)")
      assert(accumVal == 0)
    }
  }

  it should "accumulate across multiple rows sharing same LUT" in {
    test(new ComputeCore) { dut =>
      // Activations: [1, 2, 3, 4, 5, 6, 7, 8]
      // Row 0 weights: all +1 → expected 1+2+3+4+5+6+7+8 = 36
      // Row 1 weights: all -1 → expected -(1+2+3+4+5+6+7+8) = -36
      val acts = Seq(1, 2, 3, 4, 5, 6, 7, 8)
      for (i <- 0 until cfg.numPEs) {
        dut.io.activations(i).poke(acts(i).S)
      }

      // Clear rows 0 and 1
      for (r <- 0 until 2) {
        dut.io.accumClearEn.poke(true.B)
        dut.io.accumClearAddr.poke(r.U)
        dut.clock.step(1)
      }
      dut.io.accumClearEn.poke(false.B)

      // Build LUTs
      dut.io.lutBuildStart.poke(true.B)
      dut.clock.step(1)
      dut.io.lutBuildStart.poke(false.B)
      while (!dut.io.lutBuildDone.peek().litToBoolean) {
        dut.clock.step(1)
      }

      // Feed row 0 weight
      dut.io.weightData.poke(packWeights(Seq.fill(cfg.numPEs)(1)).U)
      dut.io.weightValid.poke(true.B)
      dut.io.rowIdx.poke(0.U)
      dut.clock.step(1)

      // Feed row 1 weight (back-to-back)
      dut.io.weightData.poke(packWeights(Seq.fill(cfg.numPEs)(-1)).U)
      dut.io.rowIdx.poke(1.U)
      dut.clock.step(1)
      dut.io.weightValid.poke(false.B)

      // Wait for pipeline
      dut.clock.step(cfg.tmacTreePipeStages + 4)

      // Read row 0
      dut.io.accumReadAddr.poke(0.U)
      dut.clock.step(1)
      val v0 = dut.io.accumReadData.peek().litValue.toInt
      println(s"Accumulator[0] = $v0 (expected: 36)")
      assert(v0 == 36, s"Row 0: expected 36, got $v0")

      // Read row 1
      dut.io.accumReadAddr.poke(1.U)
      dut.clock.step(1)
      val v1raw = dut.io.accumReadData.peek().litValue
      val v1 = if (v1raw >= (BigInt(1) << (cfg.accumW - 1))) (v1raw - (BigInt(1) << cfg.accumW)).toInt else v1raw.toInt
      println(s"Accumulator[1] = $v1 (expected: -36)")
      assert(v1 == -36, s"Row 1: expected -36, got $v1")
    }
  }
}
