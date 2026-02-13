package bitnet

/** Configuration for the BitNet accelerator.
  *
  * @param numPEs        Number of processing elements (must be power of 2)
  * @param activationW   Bit width of activations (INT8)
  * @param accumW        Bit width of accumulator
  * @param avalonDataW   Avalon-MM data bus width
  * @param avalonAddrW   Avalon-MM address bus width
  * @param maxDimM       Maximum output dimension M
  * @param maxDimK       Maximum reduction dimension K
  */
case class BitNetConfig(
  numPEs:       Int = 128,
  activationW:  Int = 8,
  accumW:       Int = 20,
  avalonDataW:  Int = 256,
  avalonAddrW:  Int = 32,
  maxDimM:      Int = 1024,
  maxDimK:      Int = 1024
) {
  require(isPow2(numPEs), "numPEs must be a power of 2")
  require(avalonDataW >= numPEs * 2, "Avalon data bus must fit all PE weight bits")
  require(maxDimK % numPEs == 0, "maxDimK must be divisible by numPEs for banked activation buffer")

  /** Bit width for PE output (activation + 1 for sign inversion overflow) */
  val peOutW: Int = activationW + 1

  /** Number of adder tree levels = log2(numPEs) */
  val treeDepth: Int = log2(numPEs)

  /** Adder tree output width: peOutW + treeDepth bits for accumulation growth */
  val treeOutW: Int = peOutW + treeDepth

  /** Number of pipeline stages in adder tree (one register every 2 levels) */
  val treePipeStages: Int = (treeDepth + 1) / 2

  /** Number of tiles needed for K dimension = ceil(K / numPEs) */
  def tilesK(k: Int): Int = (k + numPEs - 1) / numPEs

  /** Avalon-MM burst count width: enough bits to represent maxDimK/numPEs */
  val burstCountW: Int = log2Ceil(maxDimK / numPEs) + 1

  /** Dimension register width */
  val dimW: Int = log2Ceil(maxDimK + 1).max(log2Ceil(maxDimM + 1))

  private def isPow2(n: Int): Boolean = n > 0 && (n & (n - 1)) == 0
  private def log2(n: Int): Int = (math.log(n) / math.log(2)).toInt
  private def log2Ceil(n: Int): Int = math.ceil(math.log(n) / math.log(2)).toInt
}
