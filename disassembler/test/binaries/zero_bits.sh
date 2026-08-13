#!/usr/bin/env bash

set -e

OUT_DIR="binaries"
mkdir -p "$OUT_DIR"

python3 - "$OUT_DIR" <<'PY'
import struct
import sys

out_dir = sys.argv[1]

# Each entry is:
#   name, opcode, invalid_bits
#
# The opcode occupies bits [31:16].
#
# The following instructions have reserved zero bits:
#
#   add : bit 0
#   sub : bit 0
#   and : bit 0
#   not : bits [5:0]
#   sl  : bit 5
#   sr  : bit 5

tests = [
    ("add", 0x0008, 1 << 0),
    ("sub", 0x0020, 1 << 0),
    ("and", 0x0040, 1 << 0),
    ("not", 0x0080, 1 << 0),
    ("sl",  0x0100, 1 << 5),
    ("sr",  0x0200, 1 << 5),
]

for name, opcode, invalid_bits in tests:
    # Put the opcode in bits [31:16].
    instruction = (opcode << 16) | invalid_bits

    filename = f"{out_dir}/zero_bits_{name}.pmac"

    # PMAC stores instructions in big-endian byte order.
    bytes_ = [
        (instruction >> 24) & 0xFF,
        (instruction >> 16) & 0xFF,
        (instruction >> 8) & 0xFF,
        instruction & 0xFF,
    ]

    with open(filename, "wb") as f:
        f.write(bytes(bytes_))

    print(f"Generated: {filename}")
PY