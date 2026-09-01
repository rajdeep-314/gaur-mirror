#!/usr/bin/env bash

set -e

OUT="example/jumps.pasm"

cat > "$OUT" <<'EOF'
# Jump stress test
#
# Purpose:
#   Stress-test assembler label resolution for PC-relative
#   branches and jumps.
#
# Instructions tested:
#   - beq
#   - blt
#   - j
#
# Large blocks of SPIN instructions create large positive and
# negative jump offsets.


# Forward BEQ


beq r0 r0 beq_forward

EOF

for i in $(seq 1 29); do
    echo "spin" >> "$OUT"
done

cat >> "$OUT" <<'EOF'

beq_forward:



# Forward BLT


li r1 1
li r2 2
blt r1 r2 blt_forward

EOF

for i in $(seq 1 29); do
    echo "spin" >> "$OUT"
done

cat >> "$OUT" <<'EOF'

blt_forward:


# Forward J

j j_forward

EOF

for i in $(seq 1 32766); do
    echo "spin" >> "$OUT"
done

cat >> "$OUT" <<'EOF'

j_forward:


# Backward BEQ


beq_backward:
spin
EOF

for i in $(seq 1 29); do
    echo "spin" >> "$OUT"
done

cat >> "$OUT" <<'EOF'

beq r0 r0 beq_backward



# Backward BLT


blt_backward:
spin
EOF

for i in $(seq 1 29); do
    echo "spin" >> "$OUT"
done

cat >> "$OUT" <<'EOF'

li r1 1
li r2 2
blt r1 r2 blt_backward



# Backward J


j_backward:
spin
EOF

for i in $(seq 1 32766); do
    echo "spin" >> "$OUT"
done

cat >> "$OUT" <<'EOF'

j j_backward
EOF

echo "Generated: $OUT"