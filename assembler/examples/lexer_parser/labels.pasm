# labels.pasm

# Test labels and label references.

start:
li r1 10

loop:
addi r1 r1 -1
beq r1 r0 done
j loop

done:
spin