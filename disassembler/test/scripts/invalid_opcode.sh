#!/usr/bin/env bash

set -e

if [[ $# -ne 1 ]]; then
    echo "Usage: $0 <opcode>" >&2
    exit 1
fi

OPCODE="$1"

# Validate that the argument is a 16-bit hexadecimal/decimal value.
if ! [[ "$OPCODE" =~ ^(0[xX])?[0-9a-fA-F]+$ ]]; then
    echo "Error: opcode must be a 16-bit value" >&2
    exit 1
fi

# Convert to an integer.
OPCODE=$((OPCODE))

if (( OPCODE < 0 || OPCODE > 0xFFFF )); then
    echo "Error: opcode must be a 16-bit value" >&2
    exit 1
fi

# Put the 16-bit opcode in bits [31:16].
INSTRUCTION=$((OPCODE << 16))

B0=$(( (INSTRUCTION >> 24) & 0xFF ))
B1=$(( (INSTRUCTION >> 16) & 0xFF ))
B2=$(( (INSTRUCTION >> 8)  & 0xFF ))
B3=$(( INSTRUCTION & 0xFF ))

# Write the instruction as four big-endian bytes.
printf '%b%b%b%b' \
    "\\$(printf '%03o' "$B0")" \
    "\\$(printf '%03o' "$B1")" \
    "\\$(printf '%03o' "$B2")" \
    "\\$(printf '%03o' "$B3")"