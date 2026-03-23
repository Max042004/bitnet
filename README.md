# BitNet T-MAC FPGA Inference Accelerator

A DSP-free inference accelerator for BitNet b1.58 ternary weight networks on **DE10-Nano** (Intel Cyclone V SoC). Replaces conventional multiply-accumulate with **T-MAC (Table-based Multiply-Accumulate)** — all computation uses LUT lookups, adder trees, and BRAM only. Zero DSP blocks required.

Successfully runs **BitMamba 255M / 1B** model inference on the DE10-Nano.

## Key Idea

BitNet b1.58 constrains weights to {-1, 0, +1}. T-MAC exploits this by precomputing partial-sum lookup tables from activations, then indexing them with weight masks — replacing multiplication with table lookups:

```
Traditional:  dot(w, x) = w0*x0 + w1*x1 + w2*x2 + w3*x3     (4 muls + 3 adds)
T-MAC:        dot(w, x) = LUT[P_MASK(w)] - LUT[N_MASK(w)]    (2 lookups + 1 sub)
```

where `LUT[mask] = sum of x[i] for bit i set in mask`, `P_MASK` selects +1 positions, `N_MASK` selects -1 positions.

## System Architecture

```
  ┌──────────────────────────────────────────────────────────────────────────────────┐
  │                         DE10-Nano SoC System                                     │
  │                                                                                  │
  │  ┌──────────────┐         Lightweight Bridge          ┌───────────────────────┐  │
  │  │  HPS ARM     │◄──── Avalon-MM Slave (32-bit) ─────►│    ControlRegs        │  │
  │  │  Cortex-A9   │          (config/status)            │  ┌─────────────────┐  │  │
  │  │              │                                      │  │ START, BUSY,    │  │  │
  │  │  Linux +     │                                      │  │ DONE, DIM_M/K,  │  │  │
  │  │  bitmamba.c  │                                      │  │ WEIGHT_BASE,    │  │  │
  │  │              │                                      │  │ PERF_CYCLES     │  │  │
  │  └──────┬───────┘                                      │  └─────────────────┘  │  │
  │         │                                              └──────────┬────────────┘  │
  │         │ shared                                                  │ FSM control   │
  │         ▼                                                        ▼               │
  │  ┌──────────────┐    FPGA-to-SDRAM Bridge       ┌────────────────────────────┐   │
  │  │              │◄── Avalon-MM Master (128-bit) ─┤   BitNet T-MAC Accelerator│   │
  │  │   DDR3       │    (read weights,              │                            │   │
  │  │   Memory     │     load activations,          │         ┌──────────┐       │   │
  │  │              │     write results)             │         │   FSM    │       │   │
  │  │  ┌────────┐  │                                │         │ 12-state │       │   │
  │  │  │ Weight │  │                                │         └─────┬────┘       │   │
  │  │  │ Matrix │  │                                │               │            │   │
  │  │  │ (2-bit │  │                                │     ┌─────────┼─────────┐  │   │
  │  │  │packed) │  │                                │     ▼         ▼         ▼  │   │
  │  │  └────────┘  │                                │  LoadAct  Compute   WriteRes│  │
  │  │  ┌────────┐  │                                │   phase    phase     phase  │   │
  │  │  │  Act/  │  │                                │                            │   │
  │  │  │  Res   │  │                                └────────────────────────────┘   │
  │  │  │ buffer │  │                                                                 │
  │  │  └────────┘  │                                                                 │
  │  └──────────────┘                                                                 │
  └──────────────────────────────────────────────────────────────────────────────────┘
```

## T-MAC Compute Pipeline (Detail)

```
  Activation Buffer                         DDR3 Weight Stream
  (128-bank BRAM)                           (pipelined sub-bursts)
        │                                          │
        │ 128× INT8 activations                    │ 256-bit packed weights
        │ (32 groups × 4 per group)                │ (128 weights × 2-bit)
        ▼                                          ▼
  ┌──────────────┐                        ┌──────────────────┐
  │ TmacLutBuilder│                        │ TmacWeightIndexer│
  │              │                        │                  │
  │ Per group g: │    32× LUT[16]         │ Per group g:     │
  │ Build 16-    │──── (register ────────►│ Extract P_MASK,  │
  │ entry LUT    │      file)             │ N_MASK from 2-bit│
  │ in 4 cycles  │                        │ packed weights   │
  │              │                        │                  │
  │ lut[mask] =  │                        │ result[g] =      │
  │  Σ x[i] for  │                        │  LUT[P] - LUT[N] │
  │  bit i set   │                        │                  │
  └──────────────┘                        │ 1 pipeline reg   │
                                          └────────┬─────────┘
                                                   │
                                          32× group results (12-bit signed)
                                                   │
                                                   ▼
                                          ┌──────────────────┐
                                          │    AdderTree      │
                                          │   (32 → 1)       │
                                          │                   │
                                          │  5-level binary   │
                                          │  reduction with   │
                                          │  1 pipeline reg   │
                                          │  per level        │
                                          │                   │
                                          │  Latency: 5 cycles│
                                          └────────┬──────────┘
                                                   │
                                              21-bit signed sum
                                                   │
                                                   ▼
                                          ┌──────────────────┐
                                          │ AccumulatorArray  │
                                          │  (BRAM-backed)    │
                                          │                   │
                                          │ 1024 entries ×    │
                                          │ 21-bit SyncReadMem│
                                          │                   │
                                          │ Read-Modify-Write:│
                                          │ accum[m] +=       │
                                          │   treeOutput      │
                                          └────────┬──────────┘
                                                   │
                                          After all K-tiles:
                                          raw 32-bit accum values
                                                   │
                                                   ▼
                                          ┌──────────────────┐
                                          │  Result Buffer    │
                                          │  (SyncReadMem)    │
                                          │                   │
                                          │  → LW Bridge read │
                                          │  → DDR3 writeback │
                                          └──────────────────┘
```

## Tile-Major Dataflow

T-MAC uses a **tile-major** computation order to amortize LUT build cost:

```
For each K-tile position k (0 .. ceil(K/128)-1):
  ┌─────────────────────────────────────────────────────────┐
  │ 1. Build LUTs from activations[k*128 .. (k+1)*128-1]   │  4 cycles (once)
  │    → 32 groups × 16-entry LUT                          │
  └─────────────────────────────────────────────────────────┘
                          │
                          ▼
  ┌─────────────────────────────────────────────────────────┐
  │ 2. Stream M weight tiles from DDR3                      │  M cycles
  │    For each row m (0 .. M-1):                           │
  │      weight_tile[k][m] → WeightIndexer → AdderTree      │
  │      → accum[m] += tile_sum                             │
  └─────────────────────────────────────────────────────────┘

After all tiles: accum[m] = Σ_k dot(weights[m][k_tile], activations[k_tile])
```

**Why tile-major?** LUT build cost (4 cycles) is shared across all M rows. For M=1024, overhead is only 4/1024 = 0.4%.

## FSM State Diagram

```
                    START
                      │
                      ▼
  ┌────────┐    ┌──────────┐    ┌────────────┐    ┌──────────────┐
  │ sIdle  │───►│ sLoadAct │───►│sClearAccum │───►│ sPresentTile │
  └────────┘    │(DDR3 mode│    │ zero M     │    │ set BRAM addr│
       ▲        │ only)    │    │ accumulators│    └──────┬───────┘
       │        └──────────┘    └────────────┘           │
       │                                                  ▼
  ┌────────┐    ┌──────────────┐                  ┌──────────────┐
  │ sDone  │    │ sWriteResults│                  │  sBuildLut   │
  │        │    │ (DDR3 mode   │                  │  4 cycles    │
  └────┬───┘    │  only)       │                  └──────┬───────┘
       │        └──────┬───────┘                         │
       │               │                                  ▼
  ┌────┴───────┐       │                          ┌──────────────┐
  │sCopyResults│◄──────┘                          │ sStartStream │
  │accum→result│                                  │ pulse DDR3   │
  └────────────┘                                  └──────┬───────┘
       ▲                                                  │
       │         ┌──────────┐    ┌────────────┐          │
       └─────────┤sTileNext │◄───│ sWaitFlush │◄────     │
        all tiles│next tile │    │ drain pipe │     │    ▼
        done     │or finish │    │ 8 cycles   │ ┌───────────────┐
                 └──────────┘    └────────────┘ │sStreamWeights │
                                                │ M rows/tile   │
                                                └───────────────┘
```

## Top-Level Ports

The generated module is `BitNetAccelerator` in `chisel/generated/BitNetAccelerator.sv`.

```systemverilog
module BitNetAccelerator(
  input          clock,
  input          reset,

  // Avalon-MM Slave — connect to HPS-to-FPGA Lightweight Bridge
  input  [14:0]  io_slave_address,
  input          io_slave_read,
  input          io_slave_write,
  input  [31:0]  io_slave_writedata,
  output [31:0]  io_slave_readdata,

  // Avalon-MM Master — connect to FPGA-to-SDRAM Bridge
  output [31:0]  io_master_address,
  output         io_master_read,
  output         io_master_write,
  output [127:0] io_master_writedata,
  output [15:0]  io_master_byteenable,
  input  [127:0] io_master_readdata,
  input          io_master_waitrequest,
  input          io_master_readdatavalid,
  output [4:0]   io_master_burstcount
);
```

### Avalon-MM Slave (HPS Control)

- 15-bit byte address space (32 KB)
- 32-bit data width
- Read latency: 1 (SyncReadMem for result buffer)
- Connect to **Lightweight HPS-to-FPGA bridge** in Platform Designer

### Avalon-MM Master (DDR3 Access)

- 32-bit byte address
- 128-bit data (2 beats per weight tile = 128 weights at 2 bits each)
- 5-bit burst count (up to 16-beat pipelined sub-bursts)
- Supports read (weight streaming, activation loading) and write (result writeback)
- Connect to **FPGA-to-SDRAM bridge** in Platform Designer

## Register Map

All registers are accessed through the Avalon-MM slave interface. Byte-addressed, 32-bit aligned.

| Offset | Name | R/W | Description |
|--------|------|-----|-------------|
| `0x00` | CTRL | W | Bit 0: START (auto-clears, also clears DONE). Bit 1: DDR3_MODE |
| `0x04` | STATUS | R | Bit 0: BUSY, Bit 1: DONE |
| `0x08` | WEIGHT_BASE | R/W | DDR3 byte address of weight matrix |
| `0x0C` | DIM_M | R/W | Number of output rows (max 1024) |
| `0x10` | DIM_K | R/W | Reduction dimension / input length (max 4096) |
| `0x14` | SHIFT_AMT | R/W | Requantization right-shift (unused in raw output mode) |
| `0x18` | PERF_CYCLES | R | Clock cycles elapsed during last computation |
| `0x20` | TILE_STRIDE | R/W | Byte stride between tile columns in DDR3 |
| `0x24` | ENCODING | R/W | Weight encoding mode |
| `0x28` | ACT_DDR3_BASE | R/W | DDR3 address for activation loading |
| `0x2C` | RES_DDR3_BASE | R/W | DDR3 address for result writeback |
| `0x80`+ | ACT_DATA | W | Activation buffer (stride-4): `0x80 + i*4` writes activation[i] |
| `0x4000`+ | RES_DATA | R | Result buffer (stride-4): `0x4000 + i*4` reads result[i] |

## Chisel Module Hierarchy

```
BitNetAccelerator (top)
├── ControlRegs           — Avalon-MM slave, register file, FSM control signals
├── ActivationBuffer      — 128-bank BRAM, parallel read of 128 activations per tile
├── ActivationLoader      — DDR3 bulk-load activations into ActivationBuffer
├── WeightStreamer         — DDR3 pipelined sub-burst reads, beat assembly, FIFO
├── ComputeCore           — T-MAC compute pipeline
│   ├── TmacLutBuilder    — Build 32× 16-entry LUTs in 4 cycles (butterfly)
│   ├── TmacWeightIndexer — Decode 2-bit weights → P/N masks → LUT lookup → subtract
│   ├── AdderTree         — 32-input, 5-stage pipelined binary reduction
│   └── AccumulatorArray  — 1024-entry BRAM, read-modify-write accumulation
├── ResultWriter          — Write raw 32-bit results back to DDR3
└── ResultMem             — SyncReadMem for result readback via slave
```

## Output Mode

The accelerator outputs **raw 32-bit accumulator values** (no shift/clamp). This preserves full precision for ARM-side dequantization:

```
out[i] = raw_accum[i] / (scale_x * weight_scale)
```

This approach avoids lossy INT8 requantization on the FPGA, enabling accurate inference for models like BitMamba 255M/1B where precision matters.

## Weight Packing Format

Weights are stored in DDR3 as 2-bit packed values in **tile-major** layout:

```
Bit encoding (per weight):
  00 → weight =  0   (both masks = 0)
  01 → weight = +1   (P_MASK bit set)
  10 → weight = -1   (N_MASK bit set)
  11 → reserved       (both masks = 0)

Tile-major DDR3 layout:
  [tile0_row0][tile0_row1]...[tile0_rowM-1]
  [tile1_row0][tile1_row1]...[tile1_rowM-1]
  ...

Address = WEIGHT_BASE + tileIdx * tileStride + rowIdx * beatsPerTile * 16
```

## Default Configuration

| Parameter | Value |
|-----------|-------|
| T-MAC groups | 32 (128 PEs / 4 per group) |
| Group size | 4 activations → 16-entry LUT |
| LUT entry width | 11-bit (INT8 + log2(4) + 1) |
| Group output width | 12-bit (after subtraction) |
| Adder tree | 32→1, 5 stages, 5-cycle latency |
| Accumulator width | 21-bit |
| Result buffer | 32-bit raw accumulator |
| Max M dimension | 1024 |
| Max K dimension | 4096 |
| Avalon data width | 128-bit |
| Beats per weight tile | 2 |
| Target clock | 100 MHz (PLL) |

## Resource Estimate (Cyclone V)

| Resource | Estimated Usage |
|----------|----------------|
| ALMs | ~5,000–7,000 |
| M10K blocks | ~10 (activation buffer + accumulators + weight FIFO + result buffer) |
| DSP blocks | **0** |
| Fmax target | 100 MHz |

## Platform Designer Integration (Quartus)

### 1. Add the SystemVerilog Source

Import `chisel/generated/BitNetAccelerator.sv` into the Quartus project.

### 2. Create a Platform Designer Component

| Interface | Type | Connect to |
|-----------|------|------------|
| `clock` / `reset` | Clock Input / Reset Input | System clock (100 MHz PLL output) |
| `io_slave_*` | Avalon-MM Slave | HPS Lightweight Bridge (`h2f_lw`) |
| `io_master_*` | Avalon-MM Master | FPGA-to-SDRAM Bridge (`f2sdram`) |

**Slave settings:** Address width 15, Data width 32, Read latency 1

**Master settings:** Address width 32, Data width 128, Burst count width 5, Variable read latency with `readdatavalid`

### 3. System Connections

```
HPS
 ├── h2f_lw_axi_master ──► bitnet_accel.slave
 └── f2sdram ◄── bitnet_accel.master

clk_0.clk ──► bitnet_accel.clock
clk_0.clk_reset ──► bitnet_accel.reset
```

## HPS Software Usage

```c
#include "bitnet_fpga.h"

// Initialize: map lightweight bridge + DDR3 weight region
fpga_init(0x30000000, 0x00100000);

// Load pre-converted FPGA weight binary into DDR3
fpga_load_weights("model_fpga.bin");

// Full float-to-float BitLinear with ARM-side quant/dequant + M-tiling
float x[K], out[M], norm_weight[K];
bitlinear_forward_fpga(x, K, M, norm_weight, weight_base, weight_scale, stride, out);

// Cleanup
fpga_cleanup();
```

The driver handles M-tiling automatically: for M > 1024, it splits across multiple FPGA invocations with pipelined dequantization overlap.

## Building from Chisel Source

Requires sbt and Java 11+.

```bash
cd chisel
sbt compile                          # Compile
sbt test                             # Run all tests
sbt "runMain bitnet.BitNetAccelMain" # Regenerate SystemVerilog
```

Output: `chisel/generated/BitNetAccelerator.sv`

## License

See repository for license details.
