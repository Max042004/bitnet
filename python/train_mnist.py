#!/usr/bin/env python3
"""
Train a BitNet b1.58 MNIST model and export weights for FPGA deployment.

Architecture: 784 -> 256 -> 128 -> 10 (3-layer MLP with ternary weights)
Exports C header files with packed weights and test data.
"""

import os
import math
import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as F
from torchvision import datasets, transforms

# ---------- Output paths ----------
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
OUTPUT_DIR = os.path.join(SCRIPT_DIR, "..", "..", "software", "mnist", "generated")

# ---------- Ternary quantization (STE) ----------

class TernaryQuantize(torch.autograd.Function):
    """Quantize weights to {-1, 0, +1} using round(clip(w / mean(|w|), -1, 1))."""

    @staticmethod
    def forward(ctx, w):
        alpha = w.abs().mean()
        if alpha == 0:
            return torch.zeros_like(w)
        w_scaled = (w / alpha).clamp(-1, 1)
        return w_scaled.round()

    @staticmethod
    def backward(ctx, grad_output):
        return grad_output  # straight-through estimator


def ternary_quantize(w):
    return TernaryQuantize.apply(w)


# ---------- Model ----------

class TernaryLinear(nn.Module):
    def __init__(self, in_features, out_features):
        super().__init__()
        self.weight = nn.Parameter(torch.empty(out_features, in_features))
        nn.init.kaiming_uniform_(self.weight, a=math.sqrt(5))

    def forward(self, x):
        w_q = ternary_quantize(self.weight)
        return F.linear(x, w_q)


class BitNetMNIST(nn.Module):
    def __init__(self):
        super().__init__()
        self.fc1 = TernaryLinear(784, 256)
        self.fc2 = TernaryLinear(256, 128)
        self.fc3 = TernaryLinear(128, 10)

    def forward(self, x):
        x = x.view(x.size(0), -1)      # flatten
        x = F.relu(self.fc1(x))
        x = F.relu(self.fc2(x))
        x = self.fc3(x)
        return x


# ---------- Training ----------

def train(model, device, train_loader, optimizer, epoch):
    model.train()
    total_loss = 0
    for batch_idx, (data, target) in enumerate(train_loader):
        data, target = data.to(device), target.to(device)
        optimizer.zero_grad()
        output = model(data)
        loss = F.cross_entropy(output, target)
        loss.backward()
        optimizer.step()
        total_loss += loss.item()
    avg = total_loss / len(train_loader)
    print(f"  Epoch {epoch:2d}  loss={avg:.4f}")
    return avg


def test(model, device, test_loader):
    model.eval()
    correct = 0
    total = 0
    with torch.no_grad():
        for data, target in test_loader:
            data, target = data.to(device), target.to(device)
            output = model(data)
            pred = output.argmax(dim=1)
            correct += pred.eq(target).sum().item()
            total += target.size(0)
    acc = 100.0 * correct / total
    print(f"  Test accuracy: {correct}/{total} ({acc:.2f}%)")
    return acc


# ---------- Shift calibration ----------

def calibrate_shift(model, device, cal_loader):
    """Determine per-layer shift amounts via multi-pass calibration.

    Each pass fixes the previous layers' shifts, then sweeps the dataset to
    find the max accumulator magnitude for the current layer.
    """
    model.eval()

    w1 = ternary_quantize(model.fc1.weight).detach().cpu().numpy().astype(np.int8)
    w2 = ternary_quantize(model.fc2.weight).detach().cpu().numpy().astype(np.int8)
    w3 = ternary_quantize(model.fc3.weight).detach().cpu().numpy().astype(np.int8)

    def collect_int8_inputs(loader):
        """Collect all test images as INT8 vectors."""
        all_acts = []
        with torch.no_grad():
            for data, _ in loader:
                x = data.view(data.size(0), -1).numpy()
                acts = (x * 127).clip(-128, 127).astype(np.int8)
                all_acts.append(acts)
        return np.concatenate(all_acts, axis=0)

    all_inputs = collect_int8_inputs(cal_loader)

    # Pass 1: determine shift for layer 1
    max_acc1 = 0
    for a in all_inputs:
        acc = w1.astype(np.int32) @ a.astype(np.int32)
        max_acc1 = max(max_acc1, np.max(np.abs(acc)))
    s1 = find_shift_for_max(max_acc1)

    # Pass 2: with fixed s1, determine shift for layer 2
    max_acc2 = 0
    for a in all_inputs:
        acc1 = w1.astype(np.int32) @ a.astype(np.int32)
        out1 = np.maximum((acc1 >> s1).clip(-128, 127), 0).astype(np.int8)
        acc2 = w2.astype(np.int32) @ out1.astype(np.int32)
        max_acc2 = max(max_acc2, np.max(np.abs(acc2)))
    s2 = find_shift_for_max(max_acc2)

    # Pass 3: with fixed s1, s2, determine shift for layer 3
    max_acc3 = 0
    for a in all_inputs:
        acc1 = w1.astype(np.int32) @ a.astype(np.int32)
        out1 = np.maximum((acc1 >> s1).clip(-128, 127), 0).astype(np.int8)
        acc2 = w2.astype(np.int32) @ out1.astype(np.int32)
        out2 = np.maximum((acc2 >> s2).clip(-128, 127), 0).astype(np.int8)
        acc3 = w3.astype(np.int32) @ out2.astype(np.int32)
        max_acc3 = max(max_acc3, np.max(np.abs(acc3)))
    s3 = find_shift_for_max(max_acc3)

    shifts = [s1, s2, s3]
    max_accs = [max_acc1, max_acc2, max_acc3]
    for i in range(3):
        print(f"  Layer {i+1}: max|acc|={max_accs[i]}, shift={shifts[i]}")

    return shifts


def find_shift_for_max(max_val):
    """Find minimum shift so that max_val >> shift fits in [-128, 127]."""
    if max_val == 0:
        return 0
    shift = 0
    while (max_val >> shift) > 127:
        shift += 1
    return shift


# ---------- Export C headers ----------

def export_weights_header(model, shifts, filepath):
    """Export ternary weights as both int8 arrays and 2-bit packed uint32 arrays."""
    w1 = ternary_quantize(model.fc1.weight).detach().cpu().numpy().astype(np.int8)
    w2 = ternary_quantize(model.fc2.weight).detach().cpu().numpy().astype(np.int8)
    w3 = ternary_quantize(model.fc3.weight).detach().cpu().numpy().astype(np.int8)

    layers = [
        ("L1", w1, 784, 256, shifts[0]),
        ("L2", w2, 256, 128, shifts[1]),
        ("L3", w3, 128, 10,  shifts[2]),
    ]

    with open(filepath, "w") as f:
        f.write("/* Auto-generated by train_mnist.py — do not edit */\n")
        f.write("#ifndef MNIST_WEIGHTS_H\n")
        f.write("#define MNIST_WEIGHTS_H\n\n")
        f.write("#include <stdint.h>\n\n")

        # Layer dimension constants
        for name, w, k, m, shift in layers:
            f.write(f"#define {name}_M {m}\n")
            f.write(f"#define {name}_K {k}\n")
            f.write(f"#define {name}_SHIFT {shift}\n")
        f.write("\n")

        # Ternary weight arrays (int8_t, for ARM reference computation)
        for name, w, k, m, shift in layers:
            f.write(f"/* {name}: {m} x {k} ternary weights (row-major) */\n")
            f.write(f"static const int8_t {name.lower()}_weights[{m * k}] = {{\n")
            for row in range(m):
                vals = w[row]
                line = ", ".join(str(int(v)) for v in vals)
                comma = "," if row < m - 1 else ""
                f.write(f"  {line}{comma}\n")
            f.write("};\n\n")

        # 2-bit packed weight arrays (uint32_t, for DDR3/FPGA)
        for name, w, k, m, shift in layers:
            tiles_per_row = (k + 127) // 128
            total_words = m * tiles_per_row * 8  # 8 uint32 per 256-bit beat
            f.write(f"/* {name}: 2-bit packed weights for DDR3 "
                    f"({m} rows x {tiles_per_row} tiles x 4 words) */\n")
            f.write(f"static const uint32_t {name.lower()}_packed[{total_words}] = {{\n")

            word_list = []
            for row in range(m):
                for tile in range(tiles_per_row):
                    packed = [0, 0, 0, 0, 0, 0, 0, 0]
                    for i in range(128):
                        col = tile * 128 + i
                        val = int(w[row, col]) if col < k else 0
                        if val == 1:
                            enc = 0x1
                        elif val == -1:
                            enc = 0x2
                        else:
                            enc = 0x0
                        packed[i // 16] |= enc << ((i % 16) * 2)
                    word_list.extend(packed)

            # Write 8 words per line
            for i in range(0, len(word_list), 8):
                chunk = word_list[i:i+8]
                line = ", ".join(f"0x{v:08X}" for v in chunk)
                comma = "," if i + 8 < len(word_list) else ""
                f.write(f"  {line}{comma}\n")

            f.write("};\n\n")

        # DDR3 byte sizes for each layer (for computing offsets)
        f.write("/* DDR3 byte sizes per layer (for offset computation) */\n")
        for name, w, k, m, shift in layers:
            tiles_per_row = (k + 127) // 128
            byte_size = m * tiles_per_row * 32  # 32 bytes per 256-bit beat
            f.write(f"#define {name}_DDR3_BYTES {byte_size}\n")
        f.write("\n")

        f.write("#endif /* MNIST_WEIGHTS_H */\n")

    print(f"  Wrote {filepath}")


def export_test_data_header(test_loader, num_images, filepath):
    """Export test images and labels as C arrays."""
    images = []
    labels = []
    for data, target in test_loader:
        for i in range(data.size(0)):
            if len(images) >= num_images:
                break
            # Flatten and convert to INT8 [-128, 127]
            img = data[i].view(-1).numpy()
            img_int8 = (img * 127).clip(-128, 127).astype(np.int8)
            images.append(img_int8)
            labels.append(int(target[i]))
        if len(images) >= num_images:
            break

    with open(filepath, "w") as f:
        f.write("/* Auto-generated by train_mnist.py — do not edit */\n")
        f.write("#ifndef MNIST_TEST_DATA_H\n")
        f.write("#define MNIST_TEST_DATA_H\n\n")
        f.write("#include <stdint.h>\n\n")
        f.write(f"#define NUM_TEST_IMAGES {num_images}\n")
        f.write(f"#define IMAGE_SIZE 784\n\n")

        # Labels
        f.write(f"static const int test_labels[{num_images}] = {{\n  ")
        f.write(", ".join(str(l) for l in labels))
        f.write("\n};\n\n")

        # Images — each 784 int8 values
        f.write(f"static const int8_t test_images[{num_images}][IMAGE_SIZE] = {{\n")
        for idx, img in enumerate(images):
            comma = "," if idx < num_images - 1 else ""
            f.write("  {")
            # Write 28 values per line (one row of the image)
            for r in range(28):
                row_vals = img[r*28:(r+1)*28]
                line = ",".join(str(int(v)) for v in row_vals)
                if r < 27:
                    f.write(f"\n    {line},")
                else:
                    f.write(f"\n    {line}")
            f.write(f"\n  }}{comma}\n")
        f.write("};\n\n")

        f.write("#endif /* MNIST_TEST_DATA_H */\n")

    print(f"  Wrote {filepath}")


# ---------- Main ----------

def main():
    print("=== BitNet b1.58 MNIST Training ===\n")

    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    print(f"Device: {device}")

    # Data loaders
    transform = transforms.Compose([
        transforms.ToTensor(),           # [0, 1] float
    ])
    train_dataset = datasets.MNIST("./data", train=True, download=True, transform=transform)
    test_dataset = datasets.MNIST("./data", train=False, transform=transform)

    train_loader = torch.utils.data.DataLoader(train_dataset, batch_size=128, shuffle=True)
    test_loader = torch.utils.data.DataLoader(test_dataset, batch_size=256, shuffle=False)

    # Model
    model = BitNetMNIST().to(device)
    optimizer = torch.optim.Adam(model.parameters(), lr=1e-3)
    scheduler = torch.optim.lr_scheduler.StepLR(optimizer, step_size=8, gamma=0.5)

    # Train
    print("\nTraining (20 epochs)...")
    best_acc = 0
    for epoch in range(1, 21):
        train(model, device, train_loader, optimizer, epoch)
        acc = test(model, device, test_loader)
        scheduler.step()
        if acc > best_acc:
            best_acc = acc

    print(f"\nBest test accuracy: {best_acc:.2f}%\n")

    # Calibrate shift amounts
    print("Calibrating shift amounts...")
    cal_loader = torch.utils.data.DataLoader(test_dataset, batch_size=256, shuffle=False)
    shifts = calibrate_shift(model, device, cal_loader)

    # Verify integer inference accuracy
    print("\nVerifying integer inference...")
    verify_int8_inference(model, device, test_loader, shifts)

    # Export
    os.makedirs(OUTPUT_DIR, exist_ok=True)
    print("\nExporting C headers...")
    export_weights_header(model, shifts, os.path.join(OUTPUT_DIR, "mnist_weights.h"))
    export_test_data_header(test_loader, 100, os.path.join(OUTPUT_DIR, "mnist_test_data.h"))

    print("\nDone!")


def verify_int8_inference(model, device, test_loader, shifts):
    """Verify that integer-only inference matches float model output."""
    model.eval()
    w1 = ternary_quantize(model.fc1.weight).detach().cpu().numpy().astype(np.int8)
    w2 = ternary_quantize(model.fc2.weight).detach().cpu().numpy().astype(np.int8)
    w3 = ternary_quantize(model.fc3.weight).detach().cpu().numpy().astype(np.int8)

    correct = 0
    total = 0

    for data, target in test_loader:
        x = data.view(data.size(0), -1).numpy()
        acts = (x * 127).clip(-128, 127).astype(np.int8)

        for b in range(acts.shape[0]):
            a = acts[b]

            # Layer 1
            acc1 = w1.astype(np.int32) @ a.astype(np.int32)
            out1 = (acc1 >> shifts[0]).clip(-128, 127).astype(np.int8)
            out1 = np.maximum(out1, 0).astype(np.int8)  # ReLU

            # Layer 2
            acc2 = w2.astype(np.int32) @ out1.astype(np.int32)
            out2 = (acc2 >> shifts[1]).clip(-128, 127).astype(np.int8)
            out2 = np.maximum(out2, 0).astype(np.int8)  # ReLU

            # Layer 3
            acc3 = w3.astype(np.int32) @ out2.astype(np.int32)
            out3 = (acc3 >> shifts[2]).clip(-128, 127).astype(np.int8)

            pred = np.argmax(out3)
            if pred == int(target[b]):
                correct += 1
            total += 1

    acc = 100.0 * correct / total
    print(f"  INT8 inference accuracy: {correct}/{total} ({acc:.2f}%)")


if __name__ == "__main__":
    main()
