# forward.pasm

# Test forward jumps.
# The target labels occur after the jump instructions.

li r1 10
beq r1 r1 equal
addi r2 r2 1

equal:
li r3 20
j end

end:
spin