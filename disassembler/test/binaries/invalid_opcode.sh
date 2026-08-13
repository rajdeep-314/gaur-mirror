#!/usr/bin/env bash

set -e

OUT_DIR="binaries"
mkdir -p "$OUT_DIR"

python3 - "$OUT_DIR" <<'PY'
import sys

out_dir = sys.argv[1]

invalid_opcodes = [
    0x0000,
    0x0003,
    0x0005,
    0x000F,
    0x00FF,
    0x0300,
    0x0FFF,
    0x3000,
    0x7FFF,
    0x8000,
    0xFFFF,
]

for opcode in invalid_opcodes:
    instruction = opcode << 16

    # PMAC stores the instruction bytes in this order.
    bytes_ = [
        (instruction >> 24) & 0xFF,
        (instruction >> 16) & 0xFF,
        (instruction >> 8) & 0xFF,
        instruction & 0xFF,
    ]

    filename = f"{out_dir}/invalid_opcode_{opcode:04x}.pmac"

    with open(filename, "wb") as f:
        f.write(bytes(bytes_))

    print(f"Generated: {filename}")
PY