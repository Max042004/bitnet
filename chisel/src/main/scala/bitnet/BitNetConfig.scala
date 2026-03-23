package bitnet

/** Configuration for the BitNet accelerator.
  *
  * @param numPEs        Number of processing elements (must be power of 2)
  * @param activationW   Bit width of activations (INT8)
  * @param accumW        Bit width of accumulator
  * @param avalonDataW   Avalon-MM data bus width (must match f2sdram bridge width)
  * @param avalonAddrW   Avalon-MM address bus width
  * @param maxDimM       Maximum output dimension M
  * @param maxDimK       Maximum reduction dimension K
  */
case class BitNetConfig(
  numPEs:       Int = 128,
  activationW:  Int = 8,
  accumW:       Int = 21,
  avalonDataW:  Int = 128,
  avalonAddrW:  Int = 32,
  maxDimM:      Int = 1024,
  maxDimK:      Int = 4096,
  maxBurstLen:  Int = 16   // Max Avalon burst length per sub-burst (pipelined)
) {
  require(isPow2(numPEs), "numPEs must be a power of 2")
  require(maxDimK % numPEs == 0, "maxDimK must be divisible by numPEs for banked activation buffer")

  /** Internal weight data width: numPEs * 2 bits (one tile of 2-bit packed weights) */
  val weightDataW: Int = numPEs * 2

  /** Number of Avalon bus beats per weight tile.
    * When avalonDataW >= weightDataW: 1 beat per tile (bus wider than tile).
    * When avalonDataW < weightDataW: multiple beats assembled into one tile. */
  val beatsPerTile: Int = if (weightDataW <= avalonDataW) 1 else {
    require(weightDataW % avalonDataW == 0,
      s"weightDataW ($weightDataW) must be divisible by avalonDataW ($avalonDataW)")
    weightDataW / avalonDataW
  }

  // ---- T-MAC parameters ----

  /** Number of activations per LUT group */
  val groupSize: Int = 4
  require(numPEs % groupSize == 0, "numPEs must be divisible by groupSize")

  /** Number of T-MAC groups */
  val numGroups: Int = numPEs / groupSize

  /** Number of entries per group LUT (2^groupSize) */
  val lutEntries: Int = 1 << groupSize

  /** Bit width of each LUT entry: sum of groupSize INT8 values needs activationW + log2(groupSize) + 1 bits */
  val lutEntryW: Int = activationW + log2Ceil(groupSize) + 1

  /** Bit width of group output after lut[p_mask] - lut[n_mask] subtraction */
  val groupOutW: Int = lutEntryW + 1

  /** T-MAC adder tree depth = log2(numGroups) */
  val tmacTreeDepth: Int = log2(numGroups)

  /** T-MAC adder tree output width */
  val tmacTreeOutW: Int = groupOutW + tmacTreeDepth

  /** T-MAC adder tree pipeline stages (one register per level) */
  val tmacTreePipeStages: Int = tmacTreeDepth

  // ---- Legacy PE parameters (kept for AdderTree backward compatibility) ----

  /** Bit width for PE output (activation + 1 for sign inversion overflow) */
  val peOutW: Int = activationW + 1

  /** Number of adder tree levels = log2(numPEs) */
  val treeDepth: Int = log2(numPEs)

  /** Adder tree output width: peOutW + treeDepth bits for accumulation growth */
  val treeOutW: Int = peOutW + treeDepth

  /** Number of pipeline stages in adder tree (one register per level) */
  val treePipeStages: Int = treeDepth

  /** Number of tiles needed for K dimension = ceil(K / numPEs) */
  def tilesK(k: Int): Int = (k + numPEs - 1) / numPEs

  /** Avalon-MM burst count width: enough bits for max burst (tiles * beatsPerTile) */
  val burstCountW: Int = log2Ceil(maxDimK / numPEs * beatsPerTile) + 1

  /** Dimension register width */
  val dimW: Int = log2Ceil(maxDimK + 1).max(log2Ceil(maxDimM + 1))

  private def isPow2(n: Int): Boolean = n > 0 && (n & (n - 1)) == 0
  private def log2(n: Int): Int = (math.log(n) / math.log(2)).toInt
  private def log2Ceil(n: Int): Int = math.ceil(math.log(n) / math.log(2)).toInt
}
