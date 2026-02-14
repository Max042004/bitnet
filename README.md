# BitNet DSP-Free FPGA Inference Accelerator

A DSP-free inference accelerator for BitNet b1.58 ternary weight networks, targeting the **DE10-Nano** (Altera Cyclone V SE SoC). All computation uses LUTs, registers, and BRAM only — zero DSP blocks required.

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
                     Avalon-MM Slave (32-bit)               Avalon-MM Master (128-bit)
                           │                                         │
  HPS (ARM) ───────────────┤                             DDR3 ◄──────┘
                           │                                         │
                     ┌─────▼──────┐                          ┌───────▼────────┐
                     │ ControlRegs │                          │ WeightStreamer  │
                     └──┬──┬──┬───┘                          └───────┬────────┘
                        │  │  │                                      │ 128-bit packed weights
              ┌─────────┘  │  └──────────┐                           │
              ▼            ▼             ▼                            ▼
        ActivationBuf  ResultMem    Config/Status           ┌─────────────────┐
        (BRAM 1024×8)  (BRAM 1024×8)                       │  WeightDecoder   │
              │                                             │  (2-bit unpack)  │
              │  64× INT8 activations                       └────────┬────────┘
              ▼                                              64× {enable, sign}
        ┌──────────────────────────────────────────────────────────────┐
        │                    PEArray (64 PEs)                          │
        │  PE: enable=0 → 0  |  enable=1,sign=0 → +act  |  sign=1 → -act │
        └──────────────────────────┬───────────────────────────────────┘
                                   │ 64× 9-bit products
                              ┌────▼─────┐
                              │ AdderTree │  6-level binary reduction
                              │ (pipelined│  Registers every 2 levels
                              │  3 cycles)│  → 15-bit sum
                              └────┬─────┘
                                   │
                              ┌────▼──────┐
                              │ Accumulate│  20-bit across K-dim tiles
                              └────┬──────┘
                                   │
                              ┌────▼──────┐
                              │ Requantize│  Shift + clamp → INT8
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
  input  [11:0]  io_slave_address,
  input          io_slave_read,
  input          io_slave_write,
  input  [31:0]  io_slave_writedata,
  output [31:0]  io_slave_readdata,

  // Avalon-MM Master — connect to FPGA-to-SDRAM Bridge
  output [31:0]  io_master_address,
  output         io_master_read,
  input  [127:0] io_master_readdata,
  input          io_master_waitrequest,
  input          io_master_readdatavalid
);
```

### Avalon-MM Slave (HPS Control)

- 12-bit byte address space (4 KB)
- 32-bit data width
- Connect to **Lightweight HPS-to-FPGA bridge** in Platform Designer

### Avalon-MM Master (DDR3 Weight Reads)

- 32-bit byte address
- 128-bit read data (carries 64 weights per beat at 2 bits each)
- Connect to **FPGA-to-SDRAM bridge** in Platform Designer
- Supports `waitrequest` / `readdatavalid` handshake

## Register Map

All registers are accessed through the Avalon-MM slave interface. Byte-addressed, 32-bit aligned.

| Offset | Name | R/W | Description |
|--------|------|-----|-------------|
| `0x00` | CTRL | W | Bit 0: START — pulse 1 to begin computation (auto-clears, also clears DONE) |
| `0x04` | STATUS | R | Bit 0: BUSY, Bit 1: DONE |
| `0x08` | WEIGHT_BASE | R/W | DDR3 byte address of weight matrix |
| `0x0C` | DIM_M | R/W | Number of output rows (max 1024) |
| `0x10` | DIM_K | R/W | Reduction dimension / input length (max 2048) |
| `0x14` | SHIFT_AMT | R/W | Requantization right-shift amount (0–31) |
| `0x18` | PERF_CYCLES | R | Clock cycles elapsed during last computation |
| `0x80`–`0x207C` | ACT_DATA | W | Activation buffer — write INT8 values, stride-4 byte addressing: offset `0x80 + i*4` writes activation[i] (up to maxDimK=2048 entries) |
| `0x4000`+ | RES_DATA | R | Result buffer — read INT8 outputs, stride-4 byte addressing: offset `0x4000 + i*4` reads result[i] |

## Platform Designer Integration (Quartus)

### 1. Add the SystemVerilog Source

Import `chisel/generated/BitNetAccelerator.sv` into the Quartus project.

> **Note:** The file contains a `mem_1024x8` module that models dual-port BRAM. For synthesis, either let Quartus infer block RAM from it, or replace it with an Altera M10K IP instantiation.

### 2. Create a Platform Designer Component

Create a new component wrapping `BitNetAccelerator`:

**Interfaces to define:**

| Interface | Type | Connect to |
|-----------|------|------------|
| `clock` / `reset` | Clock Input / Reset Input | System clock (50 MHz or PLL output) |
| `io_slave_*` | Avalon-MM Slave | HPS Lightweight Bridge (`h2f_lw`) |
| `io_master_*` | Avalon-MM Master | FPGA-to-SDRAM Bridge (`f2sdram`) |

**Slave interface settings:**
- Address width: 12 bits
- Data width: 32 bits
- Read latency: 1 (due to SyncReadMem for result buffer)

**Master interface settings:**
- Address width: 32 bits
- Data width: 128 bits
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

From Linux on the HPS (C example using `/dev/mem`):

```c
#include <fcntl.h>
#include <sys/mman.h>
#include <stdint.h>

#define LW_BRIDGE_BASE  0xFF200000
#define ACCEL_OFFSET    0x0000       // adjust to match Qsys assignment
#define ACCEL_SPAN      0x1000

#define REG_CTRL        0x00
#define REG_STATUS      0x04
#define REG_WEIGHT_BASE 0x08
#define REG_DIM_M       0x0C
#define REG_DIM_K       0x10
#define REG_SHIFT_AMT   0x14
#define REG_PERF        0x18
#define ACT_BASE        0x80
#define RES_BASE        0x100

volatile uint32_t *accel;

void accel_init() {
    int fd = open("/dev/mem", O_RDWR | O_SYNC);
    accel = (volatile uint32_t *)mmap(NULL, ACCEL_SPAN,
        PROT_READ | PROT_WRITE, MAP_SHARED, fd,
        LW_BRIDGE_BASE + ACCEL_OFFSET);
}

void accel_matvec(uint32_t weight_addr, int M, int K,
                  int8_t *activations, int8_t *results,
                  int shift_amt) {
    // 1. Write activations
    for (int i = 0; i < K; i++)
        accel[(ACT_BASE + i * 4) / 4] = (uint32_t)(uint8_t)activations[i];

    // 2. Configure dimensions
    accel[REG_WEIGHT_BASE / 4] = weight_addr;
    accel[REG_DIM_M / 4]       = M;
    accel[REG_DIM_K / 4]       = K;
    accel[REG_SHIFT_AMT / 4]   = shift_amt;

    // 3. Start
    accel[REG_CTRL / 4] = 1;

    // 4. Poll for completion
    while (!(accel[REG_STATUS / 4] & 0x2))
        ;  // wait for DONE bit

    // 5. Read results
    for (int i = 0; i < M; i++)
        results[i] = (int8_t)accel[(RES_BASE + i * 4) / 4];
}
```

## Weight Packing Format

Weights are stored in DDR3 as 2-bit packed values, 64 weights per 128-bit word:

```
Bit encoding (per weight):
  00 → weight =  0   (PE disabled)
  01 → weight = +1   (PE enabled, positive)
  10 → weight = -1   (PE enabled, negative)
  11 → reserved      (PE disabled)

128-bit word layout:
  [1:0]   = weight for PE 0
  [3:2]   = weight for PE 1
  ...
  [127:126] = weight for PE 63
```

Weight matrix layout in DDR3 (row-major, M rows, each row has `ceil(K/64)` beats):

```
Address = WEIGHT_BASE + row * ceil(K/64) * 16 + tile * 16
```

## Default Configuration

| Parameter | Value |
|-----------|-------|
| Processing Elements | 64 |
| Activation width | 8-bit (INT8) |
| Accumulator width | 20-bit |
| Adder tree depth | 6 levels |
| Adder tree pipeline | 3 cycles |
| Max M dimension | 1024 |
| Max K dimension | 1024 |
| Avalon data width | 128-bit |
| Avalon address width | 32-bit |

## Resource Estimate (Cyclone V)

| Resource | Estimated Usage |
|----------|----------------|
| ALMs | ~3,000–4,000 |
| M10K blocks | 2 (activation buffer + result buffer) |
| DSP blocks | **0** |
| Fmax target | 50–100 MHz |

## Building from Chisel Source

Requires sbt and Java 11+.

```bash
cd chisel
sbt compile          # Compile
sbt test             # Run all tests
sbt "runMain bitnet.BitNetAccelMain"   # Regenerate Verilog
```

Output: `chisel/generated/BitNetAccelerator.sv`

## License

See repository for license details.
