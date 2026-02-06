# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

DSP-Free FPGA Inference Accelerator targeting DE10-Nano (Altera Cyclone V). This project implements BitNet b1.58's ternary weight architecture {-1, 0, +1} to eliminate DSP dependency, enabling edge AI inference using only LUTs, registers, and BRAM.

### Key Innovation

BitNet's ternary weights transform multiplication into:
- `weight = 0` → output 0 (skip)
- `weight = +1` → output activation (pass-through)
- `weight = -1` → output -activation (2's complement)

This eliminates the need for DSP blocks entirely, making low-end FPGAs viable for LLM inference.

### Target Platform

**DE10-Nano Kit** (Altera Cyclone V SE SoC):
- FPGA fabric with LUTs, BRAM (M10K ~5Mb total)
- ARM HPS with shared DDR3 access
- FPGA-to-SDRAM bridge (Avalon-MM/AXI, 128/256-bit burst)

## Project Structure

```
bitnet/
├── chisel/                 # Chisel RTL source
│   ├── src/main/scala/     # BitNet accelerator modules
│   ├── src/test/scala/     # Test benches
│   └── generated/          # Generated Verilog output
└── reference/              # Reference implementations
```

## Build Commands

```bash
cd chisel

sbt compile                          # Compile Chisel
sbt test                             # Run all tests
sbt "testOnly bitnet.*"              # Run specific tests
sbt "runMain bitnet.BitNetAccelMain" # Generate Verilog
```

Output: `generated/*.v` → Transfer to Quartus machine for FPGA integration

## References

| Reference | Purpose |
|-----------|---------|
| `reference/riscv-ai-accelerator/` | BitNet accelerator design in Chisel |
| `reference/CycloneVSoC-examples/` | FPGA-to-HPS bridge and DMA examples |

## Architecture Design

### Processing Element (PE)

Each PE is DSP-free, constructed from:
- **Zero Gating**: AND gate when Enable=0
- **Sign Inversion**: XOR + Carry-in for 2's complement
- **Adder**: Pure INT8 addition

### Memory Strategy

- **Weights**: DDR3 resident, streamed via FPGA-SDRAM bridge (tile-based burst)
- **Activations**: BRAM line/tile buffers for sliding window operations
- **KV Cache**: DDR3 (too large for BRAM), HPS manages indexing

### Data Flow

1. Weights stored in DDR3 as 2-bit packed format
2. Streamed through 128/256-bit burst reads
3. Real-time decode to Enable + Sign control signals
4. PE array processes activations with add/subtract only
5. LUT-based adder tree for parallel accumulation
6. Shift + Clamp for INT8 re-quantization

## Development Notes

### Tool Dependencies

- **sbt**: Scala build tool
- **Java 11+**: Required for Chisel/Scala
- **Verilator**: Simulation and waveform generation

### Key Design Constraints

- No DSP block usage - all computation via LUT logic
- Memory bandwidth is the primary bottleneck (not compute)
- Target sub-100mW power envelope for edge deployment
- Weight streaming preferred over caching (limited BRAM)
