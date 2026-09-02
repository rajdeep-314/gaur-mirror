#!/usr/bin/env bash

set -e

if [[ $# -ne 4 ]]; then
    echo "Usage: $0 <forward|backward> <beq|blt|j> <offset> <output>" >&2
    exit 1
fi

DIRECTION="$1"
INSTRUCTION="$2"
OFFSET="$3"
OUT="$4"

if [[ "$DIRECTION" != "forward" && "$DIRECTION" != "backward" ]]; then
    echo "Error: direction must be 'forward' or 'backward'" >&2
    exit 1
fi

if [[ "$INSTRUCTION" != "beq" &&
      "$INSTRUCTION" != "blt" &&
      "$INSTRUCTION" != "j" ]]; then
    echo "Error: instruction must be 'beq', 'blt', or 'j'" >&2
    exit 1
fi

if ! [[ "$OFFSET" =~ ^[0-9]+$ ]]; then
    echo "Error: offset must be a non-negative integer" >&2
    exit 1
fi

case "$INSTRUCTION" in
    beq|blt)
        MAX_FORWARD=31
        MAX_BACKWARD=32
        ;;
    j)
        MAX_FORWARD=32767
        MAX_BACKWARD=32768
        ;;
esac

if [[ "$DIRECTION" == "forward" ]]; then
    if (( OFFSET > MAX_FORWARD )); then
        echo "Error: maximum forward offset for $INSTRUCTION is $MAX_FORWARD" >&2
        exit 1
    fi
else
    if (( OFFSET > MAX_BACKWARD )); then
        echo "Error: maximum backward offset for $INSTRUCTION is -$MAX_BACKWARD" >&2
        exit 1
    fi
fi

if [[ "$DIRECTION" == "forward" ]]; then

    echo "# Jump stress test" > "$OUT"
    echo >> "$OUT"

    case "$INSTRUCTION" in
        beq)
            echo "beq r0 r0 target" >> "$OUT"
            ;;
        blt)
            echo "li r1 1" >> "$OUT"
            echo "li r2 2" >> "$OUT"
            echo "blt r1 r2 target" >> "$OUT"
            ;;
        j)
            echo "j target" >> "$OUT"
            ;;
    esac

    SPINS=$(( OFFSET - 1 ))

    for ((i = 0; i < SPINS; i++)); do
        echo "spin" >> "$OUT"
    done

    echo "target:" >> "$OUT"

else

    echo "# Jump stress test" > "$OUT"
    echo >> "$OUT"
    echo "target:" >> "$OUT"

    SPINS=$(( OFFSET ))

    for ((i = 0; i < SPINS; i++)); do
        echo "spin" >> "$OUT"
    done

    case "$INSTRUCTION" in
        beq)
            echo "beq r0 r0 target" >> "$OUT"
            ;;
        blt)
            echo "li r1 1" >> "$OUT"
            echo "li r2 2" >> "$OUT"
            echo "blt r1 r2 target" >> "$OUT"
            ;;
        j)
            echo "j target" >> "$OUT"
            ;;
    esac

fi
