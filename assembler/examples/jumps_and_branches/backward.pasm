# backward.pasm

# Test backward jumps.
# The jump targets occur before the jump instructions.

start:
li r1 10
addi r1 r1 -1
j start