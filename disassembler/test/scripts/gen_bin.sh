#!/usr/bin/env bash

set -e

if [[ $# -ne 2 ]]; then
    echo "Usage: $0 <opcode> <offset>" >&2
    exit 1
fi

opcode=$(( $1 ))
offset=$(( $2 ))

if (( opcode < 0 || opcode > 0xFFFF )); then
    echo "Error: opcode must be a 16-bit value" >&2
    exit 1
fi

if (( offset < 0 || offset > 0xFFFF )); then
    echo "Error: offset must be a 16-bit value" >&2
    exit 1
fi

instruction=$(( (opcode << 16) | offset ))

b0=$(( (instruction >> 24) & 0xFF ))
b1=$(( (instruction >> 16) & 0xFF ))
b2=$(( (instruction >> 8) & 0xFF ))
b3=$(( instruction & 0xFF ))

printf '%b' \
    "$(printf '\\x%02x\\x%02x\\x%02x\\x%02x' "$b0" "$b1" "$b2" "$b3")"