# T-MAC FPGA Inference Accelerator (DSP-Free, BitNet b1.58)

A **DSP-free, table-based** inference accelerator for BitNet b1.58 ternary weight networks, targeting the **DE10-Nano** (Altera Cyclone V SE SoC). All computation uses LUTs, registers, and BRAM only — **zero DSP blocks**.

The accelerator uses **T-MAC (Table-based MAC)**: instead of per-element multiply-accumulate, it pre-computes 16 possible linear combinations of every 3 activations into a LUT, then walks the weight matrix as a stream of 4-bit nibble indices + 1-bit signs. The main loop becomes "look up + conditional negate + add" — no multipliers, no DSP blocks.

Primary target model: **microsoft/bitnet-b1.58-2B-4T**. Secondary: **BitMamba 1B**.

## Key Idea

For every group of 3 activations `(a0, a1, a2)`, there are only 16 distinct partial sums of the form `±a0 ± a1 ± a2` (plus zero). Pre-computing these into a LUT once per layer means each weight group only needs:

1. A 4-bit nibble → indexes one of 16 LUT entries
2. A 1-bit sign  → conditional 2's-complement negate
3. Add into the running accumulator

| Operation | Hardware cost |
|-----------|---------------|
| LUT entry lookup | 16:1 MUX on a 256-bit word |
| Sign correction  | XOR + carry-in (2's complement) |
| Accumulate       | Plain INT16/INT32 adder |
| **Multiplier**   | **None** |

## Architecture

```
                Avalon-MM Slave (32-bit)            Avalon-MM Master (128-bit)
                       │                                       │
  HPS (ARM) ───────────┤                            DDR3 ◄─────┘
                       │                                       │
                  ┌────▼─────┐                       ┌─────────▼──────────┐
                  │ CtrlRegs │                       │ TMacWeightStreamer │
                  └──┬───┬───┘                       │  • nibBuf A/B      │
                     │   │                           │  • signBuf A/B     │
                     ▼   ▼                           │  • per-row stride  │
              ActBuf  ResBuf                         │    accumulator     │
            (BRAM)  (BRAM)                           └─────────┬──────────┘
                │                                              │
                │   3× INT8 act per group           nibble + sign streams
                ▼                                              │
          ┌──────────────┐                            ┌────────▼─────────┐
          │  LutBuilder  │── 16-entry LUT word ──────►│   LutBram (banks)│
          │  (3-stage:   │                            └────────┬─────────┘
          │   read →     │                                     │
          │   compute →  │                            ┌────────▼─────────┐
          │   write)     │                            │ TMacComputeCore  │
          └──────────────┘                            │  • 32 engines    │
                                                      │  • Stage 2a: MUX │
                                                      │  • Stage 2b: sgn │
                                                      │  • 6-level adder │
                                                      │    tree          │
                                                      │  • Row accum     │
                                                      └────────┬─────────┘
                                                               │
                                                       ┌───────▼────────┐
                                                       │  Requantize    │
                                                       │  (shift+clamp) │
                                                       └───────┬────────┘
                                                               ▼
                                                          Result BRAM
```

## Top-Level Module

The generated module is `TMacAccelerator` in `chisel/generated/TMacAccelerator.sv`.

```systemverilog
module TMacAccelerator(
  input          clock,
  input          reset,

  // Avalon-MM Slave — HPS Lightweight Bridge
  input  [14:0]  io_slave_address,
  input          io_slave_read,
  input          io_slave_write,
  input  [31:0]  io_slave_writedata,
  output [31:0]  io_slave_readdata,

  // Avalon-MM Master — FPGA-to-SDRAM Bridge (128-bit)
  output [31:0]  io_master_address,
  output         io_master_read,
  output         io_master_write,
  output [127:0] io_master_writedata,
  input  [127:0] io_master_readdata,
  input          io_master_waitrequest,
  input          io_master_readdatavalid,
  output [4:0]   io_master_burstcount
);
```

## Register Map

| Offset | Name | R/W | Description |
|--------|------|-----|-------------|
| `0x00` | CTRL          | W   | Bit 0: START pulse |
| `0x04` | STATUS        | R   | Bit 0: BUSY, Bit 1: DONE |
| `0x08` | WEIGHT_BASE   | R/W | DDR3 byte address of nibble array |
| `0x0C` | DIM_M         | R/W | Number of output rows |
| `0x10` | DIM_K         | R/W | Reduction dimension (max 4096) |
| `0x14` | SHIFT_AMT     | R/W | Requantization right-shift |
| `0x18` | PERF_CYCLES   | R   | Cycles of last run |
| `0x1C` | **DIM_N3**    | R/W | **HPS-supplied K/3** — eliminates the on-chip non-power-of-2 divider that previously dominated the critical path |
| `0x20` | WEIGHTS_PER_BEAT | R/W | (legacy) |
| `0x24` | ENCODING_MODE | R/W | (legacy) |
| `0x28` | ACT_DDR3_BASE | R/W | DDR3 base for activation DMA |
| `0x80`+  | ACT_DATA  | W | Direct activation register write (PIO mode) |
| `0x4000`+| RES_DATA  | R | Result buffer read |

## 100 MHz Timing Closure

The accelerator runs at **100 MHz** on Cyclone V `5CSEBA6U23I7`. Closing timing required several micro-architectural rewrites of the worst critical paths reported by TimeQuest:

| Optimization | Why |
|--------------|-----|
| **HPS supplies `DIM_N3 = K/3`** | Removed a 17-level combinational divider (non-power-of-2). Setup slack improved from −19.846 ns → −4.744 ns; Fmax 34 MHz → 67 MHz. |
| **WeightStreamer per-row stride accumulator** | Eliminated `rowIdx × tilesPerRow` and `rowIdx × signBeats` variable×variable LUT multipliers. Replaced by `base += stride` each row. |
| **LutBuilder three-stage pipeline (`sRead → sCompute → sWrite`)** | Broke a BRAM → 16 INT16 adders → BRAM single-cycle path. The 16 LUT entries are now registered before the LUT BRAM write. |
| **TMacComputeCore split LUT MUX / sign correction** | The 16:1 MUX on a 256-bit LUT word + the conditional negate were too long for a single 10 ns cycle. Now in two pipeline stages. |
| **QSys clock declaration 50 → 100 MHz** | The PLL was producing 100 MHz but `clk_0` was declared as 50 MHz, mismatching STA constraints. |

The full pipeline depth is 10 stages (BRAM read + nibble/sign align + LUT MUX reg + sign reg + 6-level adder tree + accumulator).

## Default Configuration

| Parameter | Value |
|-----------|-------|
| T-MAC engines | 32 (parallel LUT lookups per cycle) |
| Group size    | 3 (activations per LUT) |
| LUT entries / group | 16 (4-bit nibble index) |
| LUT entry width | 16-bit signed |
| Activation width | INT8 |
| Adder tree | 6 levels, fully pipelined |
| Avalon data width | 128-bit |
| Avalon address width | 32-bit |
| Burst count width | 5-bit (max 16-beat bursts) |
| Max M | 1024 |
| Max K | **4096** (sized for BitMamba 1B `out_proj`) |
| Target clock | 100 MHz |
| **DSP blocks** | **0** |

## Module Map

| Module | Role |
|--------|------|
| `TMacAccelerator` | Top-level FSM, row iteration, slave wiring |
| `TMacControlRegs` | Avalon-MM slave + register file (incl. `DIM_N3`) |
| `TMacWeightStreamer` | DDR3 prefetch with `nibBuf A/B` + `signBuf A/B`, per-row stride accumulator (no multipliers) |
| `LutBuilder` | 3-stage pipelined LUT construction from activation triples |
| `LutBram` | Multi-bank LUT storage feeding the compute core |
| `TMacComputeCore` | 32-engine parallel LUT lookup + sign correction + 6-level adder tree + row accumulator |
| `ActivationBuffer` | INT8 activation BRAM |
| `ResultBuffer` | Output BRAM (raw accumulator or requantized) |
| `Requantizer` | `(acc >> shift).clamp(-128, 127)` |
| `AvalonMMReadMaster` / `AvalonMMWriteMaster` | 128-bit AXI/Avalon protocol wrappers |

## Verification

```bash
cd chisel
sbt test                                    # 8 suites, 82 tests
sbt "testOnly bitnet.LutBuilderTest"        # single suite
sbt "runMain bitnet.TMacAccelMain"          # regenerate SystemVerilog
```

All 82 ScalaTest cases pass on the current `turbo` branch. `LutBuilderTest` cross-checks every 16-entry LUT against the `biturbo.c` reference implementation across multiple banks.

## Platform Designer Integration

1. Import `chisel/generated/TMacAccelerator.sv` into the Quartus project.
2. Wrap it as a Platform Designer component with one Avalon-MM slave (15-bit addr, 32-bit data, read latency 1) and one Avalon-MM master (32-bit addr, 128-bit data, variable latency, write enabled).
3. Connect:
   - `slave` → `hps_0.h2f_lw_axi_master`
   - `master` → `hps_0.f2sdram` bridge
   - `clock` ← 100 MHz PLL output (declare `clk_0.clockFrequency = 100000000`)
4. Assign the slave a base address inside the lightweight bridge (default `0xFF200000`).

## HPS Software Usage

```c
#include "bitnet_fpga.h"

fpga_init(0x30000000, 0x00100000);
fpga_load_weights("model_fpga.bin");

// Configure dimensions — N3 = K/3 must be supplied by HPS
fpga_reg_write(REG_DIM_M,  M);
fpga_reg_write(REG_DIM_K,  K);
fpga_reg_write(REG_DIM_N3, K / 3);          // ← required

fpga_bitlinear(activations, K, weight_base, M, stride, results);
fpga_cleanup();
```

## License

See repository for license details.
