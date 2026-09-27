# decimal.pasm

# Test decimal integer literals.
# Both positive and negative values should be accepted.

apc r1 42
apc r2 -42
addi r3 r1 10
addi r4 r2 -10
