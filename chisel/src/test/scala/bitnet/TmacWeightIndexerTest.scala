package bitnet

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class TmacWeightIndexerTest extends AnyFlatSpec with ChiselScalatestTester {
  implicit val cfg: BitNetConfig = BitNetConfig(numPEs = 8, avalonDataW = 16, maxDimK = 8, maxDimM = 4)

  behavior of "TmacWeightIndexer"

  /** Pack weights for all PEs into a single packed word.
    * Each weight: 0→00, +1→01, -1→10
    */
  def packWeights(weights: Seq[Int]): BigInt = {
    var packed = BigInt(0)
    for (i <- weights.indices) {
      val enc = weights(i) match {
        case 0  => 0
        case 1  => 1
        case -1 => 2
      }
      packed = packed | (BigInt(enc) << (i * 2))
    }
    packed
  }

  it should "compute correct group results for all-+1 weights" in {
    test(new TmacWeightIndexer) { dut =>
      // Set up LUTs for 2 groups: activations [1,2,3,4] and [5,6,7,8]
      // Group 0 LUT: subset sums of [1,2,3,4]
      // Group 1 LUT: subset sums of [5,6,7,8]
      val acts0 = Seq(1, 2, 3, 4)
      val acts1 = Seq(5, 6, 7, 8)

      for (g <- 0 until cfg.numGroups) {
        val acts = if (g == 0) acts0 else acts1
        for (e <- 0 until cfg.lutEntries) {
          var sum = 0
          for (i <- 0 until 4) if ((e & (1 << i)) != 0) sum += acts(i)
          dut.io.luts(g)(e).poke(sum.S)
        }
      }

      // All weights +1: p_mask = 0b1111 = 15, n_mask = 0b0000 = 0
      val weights = Seq.fill(cfg.numPEs)(1)
      dut.io.packed.poke(packWeights(weights).U)
      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.valid_in.poke(false.B)

      // After 1 pipeline register, results should be valid
      dut.io.valid_out.expect(true.B)
      // Group 0: lut[15] - lut[0] = (1+2+3+4) - 0 = 10
      dut.io.groupResults(0).expect(10.S)
      // Group 1: lut[15] - lut[0] = (5+6+7+8) - 0 = 26
      dut.io.groupResults(1).expect(26.S)
    }
  }

  it should "compute correct group results for all -1 weights" in {
    test(new TmacWeightIndexer) { dut =>
      val acts0 = Seq(1, 2, 3, 4)
      for (g <- 0 until cfg.numGroups) {
        for (e <- 0 until cfg.lutEntries) {
          var sum = 0
          for (i <- 0 until 4) if ((e & (1 << i)) != 0) sum += acts0(i)
          dut.io.luts(g)(e).poke(sum.S)
        }
      }

      // All weights -1: p_mask = 0, n_mask = 15
      val weights = Seq.fill(cfg.numPEs)(-1)
      dut.io.packed.poke(packWeights(weights).U)
      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.valid_in.poke(false.B)

      dut.io.valid_out.expect(true.B)
      // Group 0: lut[0] - lut[15] = 0 - 10 = -10
      dut.io.groupResults(0).expect(-10.S)
    }
  }

  it should "compute zero for all-zero weights" in {
    test(new TmacWeightIndexer) { dut =>
      val acts0 = Seq(10, 20, 30, 40)
      for (g <- 0 until cfg.numGroups) {
        for (e <- 0 until cfg.lutEntries) {
          var sum = 0
          for (i <- 0 until 4) if ((e & (1 << i)) != 0) sum += acts0(i)
          dut.io.luts(g)(e).poke(sum.S)
        }
      }

      // All weights 0: p_mask = 0, n_mask = 0
      val weights = Seq.fill(cfg.numPEs)(0)
      dut.io.packed.poke(packWeights(weights).U)
      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.valid_in.poke(false.B)

      dut.io.valid_out.expect(true.B)
      for (g <- 0 until cfg.numGroups) {
        dut.io.groupResults(g).expect(0.S)
      }
    }
  }

  it should "compute correct mixed weight pattern" in {
    test(new TmacWeightIndexer) { dut =>
      // Group 0: activations [10, 20, 30, 40], weights [+1, -1, 0, +1]
      // p_mask = 0b1001 = 9, n_mask = 0b0010 = 2
      // result = lut[9] - lut[2] = (10+40) - 20 = 30
      val acts0 = Seq(10, 20, 30, 40)
      for (g <- 0 until cfg.numGroups) {
        for (e <- 0 until cfg.lutEntries) {
          var sum = 0
          for (i <- 0 until 4) if ((e & (1 << i)) != 0) sum += acts0(i)
          dut.io.luts(g)(e).poke(sum.S)
        }
      }

      // Group 0: [+1, -1, 0, +1], Group 1: [0, 0, 0, 0]
      val weights = Seq(1, -1, 0, 1, 0, 0, 0, 0)
      dut.io.packed.poke(packWeights(weights).U)
      dut.io.valid_in.poke(true.B)
      dut.clock.step(1)
      dut.io.valid_in.poke(false.B)

      dut.io.valid_out.expect(true.B)
      dut.io.groupResults(0).expect(30.S)
      dut.io.groupResults(1).expect(0.S)
    }
  }
}
