# BitNet DSP-Free FPGA Inference Accelerator

A DSP-free inference accelerator for BitNet b1.58 ternary weight networks, targeting the **DE10-Nano** (Altera Cyclone V SE SoC). All computation uses LUTs, registers, and BRAM only — zero DSP blocks required.

Successfully runs **BitMamba 255M** model inference on the DE10-Nano.

## Key Idea

BitNet b1.58 constrains weights to {-1, 0, +1}. This turns multiplication into simple control logic:

| Weight | Operation |
|--------|-----------|
| 0 | Output zero (AND gate) |
| +1 | Pass activation through |
| -1 | Negate activation (2's complement) |

The result: a full matrix-vector engine that fits on low-end FPGAs with no DSP usage.

## Architecture

```
                     Avalon-MM Slave (32-bit)               Avalon-MM Master (256-bit)
                           │                                         │
  HPS (ARM) ───────────────┤                             DDR3 ◄──────┘
                           │                                         │
                     ┌─────▼──────┐                          ┌───────▼────────┐
                     │ ControlRegs │                          │ WeightStreamer  │
                     └──┬──┬──┬───┘                          │ (double-buffer │
                        │  │  │                               │  prefetch)     │
              ┌─────────┘  │  └──────────┐                   └───────┬────────┘
              ▼            ▼             ▼                            │ 256-bit packed weights
        ActivationBuf  ResultMem    Config/Status           ┌────────▼─────────┐
       (BRAM 2048×8)  (BRAM 1024×32)                       │  WeightDecoder   │
              │                                             │  (2-bit unpack)  │
              │  128× INT8 activations                      └────────┬────────┘
              ▼                                             128× {enable, sign}
        ┌──────────────────────────────────────────────────────────────┐
        │                    PEArray (128 PEs)                         │
        │  PE: enable=0 → 0  |  enable=1,sign=0 → +act  |  sign=1 → -act  │
        └──────────────────────────┬───────────────────────────────────┘
                                   │ 128× 9-bit products
                              ┌────▼─────┐
                              │ AdderTree │  7-level binary reduction
                              │ (pipelined│  Register at every level
                              │  7 cycles)│  → 16-bit sum
                              └────┬─────┘
                                   │
                              ┌────▼──────┐
                              │ Accumulate│  20-bit across K-dim tiles
                              └────┬──────┘
                                   │
                              ┌────▼──────┐
                              │ Raw 32-bit│  Full-precision output
                              │  output   │  (ARM-side dequantization)
                              └────┬──────┘
                                   │
                                   ▼
                              Result Memory
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
  input  [255:0] io_master_readdata,
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

### Avalon-MM Master (DDR3 Weight Reads)

- 32-bit byte address
- 256-bit read data (carries 128 weights per beat at 2 bits each)
- 5-bit burst count (up to 16-beat bursts)
- Connect to **FPGA-to-SDRAM bridge** in Platform Designer
- Supports `waitrequest` / `readdatavalid` handshake
- Double-buffered prefetch: overlaps DDR3 reads for row N+1 with row N compute

## Register Map

All registers are accessed through the Avalon-MM slave interface. Byte-addressed, 32-bit aligned.

| Offset | Name | R/W | Description |
|--------|------|-----|-------------|
| `0x00` | CTRL | W | Bit 0: START — pulse 1 to begin computation (auto-clears, also clears DONE) |
| `0x04` | STATUS | R | Bit 0: BUSY, Bit 1: DONE |
| `0x08` | WEIGHT_BASE | R/W | DDR3 byte address of weight matrix |
| `0x0C` | DIM_M | R/W | Number of output rows (max 1024) |
| `0x10` | DIM_K | R/W | Reduction dimension / input length (max 2048) |
| `0x14` | SHIFT_AMT | R/W | Requantization right-shift amount (0–31, unused in raw output mode) |
| `0x18` | PERF_CYCLES | R | Clock cycles elapsed during last computation |
| `0x80`–`0x207C` | ACT_DATA | W | Activation buffer — write INT8 values, stride-4 byte addressing: offset `0x80 + i*4` writes activation[i] (up to maxDimK=2048 entries) |
| `0x4000`+ | RES_DATA | R | Result buffer — read raw 32-bit accumulator outputs, stride-4 byte addressing: offset `0x4000 + i*4` reads result[i] |

## Output Mode

The accelerator outputs **raw 32-bit accumulator values** (no shift/clamp). This preserves full precision for ARM-side dequantization:

```
out[i] = raw_accum[i] / (scale_x * weight_scale)
```

This approach avoids lossy INT8 requantization on the FPGA, enabling accurate inference for models like BitMamba 255M where precision matters.

## Platform Designer Integration (Quartus)

### 1. Add the SystemVerilog Source

Import `chisel/generated/BitNetAccelerator.sv` into the Quartus project.

### 2. Create a Platform Designer Component

Create a new component wrapping `BitNetAccelerator`:

**Interfaces to define:**

| Interface | Type | Connect to |
|-----------|------|------------|
| `clock` / `reset` | Clock Input / Reset Input | System clock (100 MHz PLL output) |
| `io_slave_*` | Avalon-MM Slave | HPS Lightweight Bridge (`h2f_lw`) |
| `io_master_*` | Avalon-MM Master | FPGA-to-SDRAM Bridge (`f2sdram`) |

**Slave interface settings:**
- Address width: 15 bits
- Data width: 32 bits
- Read latency: 1 (due to SyncReadMem for result buffer)

**Master interface settings:**
- Address width: 32 bits
- Data width: 256 bits
- Burst count width: 5 bits
- Read latency: variable (use `readdatavalid` pipelining)

### 3. System Connections

In Platform Designer (Qsys):

```
HPS
 ├── h2f_lw_axi_master ──► bitnet_accel.slave
 └── f2sdram ◄── bitnet_accel.master

clk_0.clk ──► bitnet_accel.clock
clk_0.clk_reset ──► bitnet_accel.reset
```

### 4. Address Assignment

Assign the slave a base address in the HPS lightweight bridge space (default `0xFF200000` on DE10-Nano). The master addresses DDR3 directly using physical addresses.

## HPS Software Usage

From Linux on the HPS, use the FPGA driver header (`bitnet_fpga.h`):

```c
#include "bitnet_fpga.h"

// Initialize: map lightweight bridge + DDR3 weight region
fpga_init(0x30000000, 0x00100000);

// Load pre-converted FPGA weight binary into DDR3
fpga_load_weights("model_fpga.bin");

// Option A: Low-level INT8 matmul (raw 32-bit accumulator output)
int8_t activations[K];
int32_t results[M];
fpga_bitlinear(activations, K, weight_base, M, stride, results);

// Option B: Full float-to-float BitLinear with ARM-side quant/dequant
float x[K], out[M], norm_weight[K];
bitlinear_forward_fpga(x, K, M, norm_weight, weight_base, weight_scale, stride, out);

// Cleanup
fpga_cleanup();
```

The driver handles M-tiling automatically: for M > 1024, it splits across multiple FPGA invocations while reusing activations in BRAM.

## Weight Packing Format

Weights are stored in DDR3 as 2-bit packed values, 128 weights per 256-bit word:

```
Bit encoding (per weight):
  00 → weight =  0   (PE disabled)
  01 → weight = +1   (PE enabled, positive)
  10 → weight = -1   (PE enabled, negative)
  11 → reserved      (PE disabled)

256-bit word layout:
  [1:0]     = weight for PE 0
  [3:2]     = weight for PE 1
  ...
  [255:254] = weight for PE 127
```

Weight matrix layout in DDR3 (row-major, M rows, each row has `ceil(K/128)` beats):

```
Address = WEIGHT_BASE + row * ceil(K/128) * 32 + tile * 32
```

## Default Configuration

| Parameter | Value |
|-----------|-------|
| Processing Elements | 128 |
| Activation width | 8-bit (INT8) |
| Accumulator width | 20-bit |
| Result buffer | 32-bit raw accumulator |
| Adder tree depth | 7 levels |
| Adder tree pipeline | 7 cycles (1 register per level) |
| Max M dimension | 1024 |
| Max K dimension | 2048 |
| Avalon data width | 256-bit |
| Avalon address width | 32-bit |
| Target clock | 100 MHz |

## Resource Estimate (Cyclone V)

| Resource | Estimated Usage |
|----------|----------------|
| ALMs | ~5,000–7,000 |
| M10K blocks | ~10 (activation buffer + result buffer + weight FIFOs) |
| DSP blocks | **0** |
| Fmax target | 100 MHz |

## Building from Chisel Source

Requires sbt and Java 11+.

```bash
cd chisel
sbt compile                          # Compile
sbt test                             # Run all 8 test suites
sbt "runMain bitnet.BitNetAccelMain" # Regenerate SystemVerilog
```

Output: `chisel/generated/BitNetAccelerator.sv`

## License

See repository for license details.
