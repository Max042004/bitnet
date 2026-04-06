package bitnet

/** Configuration for the T-MAC FPGA accelerator.
  *
  * Replaces BitNetConfig for the T-MAC architecture, where groups of 3
  * ternary weights are encoded as (4-bit nibble, 1-bit sign) and computation
  * uses LUT lookup instead of scalar multiply-accumulate.
  *
  * @param numEngines     Number of parallel LUT lookup engines (P)
  * @param groupSize      Ternary weights per group (always 3 for T-MAC TL2)
  * @param lutEntries     LUT entries per group (16 for 3-weight groups)
  * @param lutWidth       Bit width of each LUT entry (INT16)
  * @param activationW    Bit width of activations (INT8)
  * @param accumW         Bit width of accumulator (INT32)
  * @param avalonDataW    Avalon-MM data bus width (DDR3 bridge)
  * @param avalonAddrW    Avalon-MM address bus width
  * @param maxDimM        Maximum output dimension M
  * @param maxDimK        Maximum input dimension K (must be divisible by groupSize)
  * @param maxBurstLen    Maximum Avalon burst length per sub-burst
  */
case class TMacConfig(
  numEngines:   Int = 32,
  groupSize:    Int = 3,
  lutEntries:   Int = 16,
  lutWidth:     Int = 16,
  activationW:  Int = 8,
  accumW:       Int = 32,
  avalonDataW:  Int = 128,
  avalonAddrW:  Int = 32,
  maxDimM:      Int = 8192,
  maxDimK:      Int = 8192,
  maxBurstLen:  Int = 16
) {
  require(isPow2(numEngines), "numEngines must be a power of 2")
  require(groupSize == 3, "T-MAC TL2 requires groupSize=3")

  /** Maximum number of 3-weight groups = maxDimK / groupSize */
  val maxGroups: Int = maxDimK / groupSize

  /** Maximum tiles per row = ceil(maxGroups / numEngines) */
  val maxTilesPerRow: Int = (maxGroups + numEngines - 1) / numEngines

  /** Number of activations packed per DDR3 beat */
  val actsPerBeat: Int = avalonDataW / activationW  // 16

  /** Number of activation BRAM banks (matches actsPerBeat for bulk writes) */
  val numActBanks: Int = actsPerBeat

  /** LUT BRAM bank depth = maxTilesPerRow (one word per tile per bank) */
  val lutBankDepth: Int = maxTilesPerRow

  /** LUT word width = lutEntries * lutWidth (256 bits: 16 entries * 16 bits) */
  val lutWordW: Int = lutEntries * lutWidth

  /** Engine output width after sign correction = lutWidth + 1 */
  val engineOutW: Int = lutWidth + 1

  /** Adder tree depth = log2(numEngines) */
  val treeDepth: Int = log2(numEngines)

  /** Adder tree output width = engineOutW + treeDepth */
  val treeOutW: Int = engineOutW + treeDepth

  /** Nibble bits per group = 4 */
  val nibbleW: Int = 4

  /** Nibbles per DDR3 beat = avalonDataW / nibbleW = 32 for 128-bit bus */
  val nibblesPerBeat: Int = avalonDataW / nibbleW

  /** Sign bits per DDR3 beat = avalonDataW = 128 */
  val signsPerBeat: Int = avalonDataW

  /** Max sign beats per row = ceil(maxGroups / signsPerBeat) */
  val maxSignBeatsPerRow: Int = (maxGroups + signsPerBeat - 1) / signsPerBeat

  /** Dimension register width (covers both M and K) */
  val dimW: Int = log2Ceil(maxDimK + 1).max(log2Ceil(maxDimM + 1))

  /** Burst count width for Avalon-MM master */
  val burstCountW: Int = log2Ceil(maxTilesPerRow.max(maxSignBeatsPerRow) + 1) + 1

  /** Number of 3-weight groups for dimension K */
  def numGroups(k: Int): Int = k / groupSize

  /** Number of tiles for n3 groups = ceil(n3 / numEngines) */
  def tilesPerRow(n3: Int): Int = (n3 + numEngines - 1) / numEngines

  private def isPow2(n: Int): Boolean = n > 0 && (n & (n - 1)) == 0
  private def log2(n: Int): Int = (math.log(n) / math.log(2)).toInt
  private def log2Ceil(n: Int): Int = math.ceil(math.log(n) / math.log(2)).toInt
}
