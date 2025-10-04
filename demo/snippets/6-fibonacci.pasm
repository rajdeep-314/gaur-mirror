# 6. Fibonacci sequence.
# Generate the Fibonacci sequence and store it in the RAM.
# Let RAM[0] and RAM[1] store n and base, respectively. The program should
# populate the RAM from addresses base through base + n - 1 with the first n
# elements of the Fibonacci sequence (0, 1, 1, 2, ...).


ld r1 r0 0
ld r2 r0 1
addi r3 r0 0
addi r5 r0 0
addi r6 r0 1

beq r3 r1 2
j 2
j 8
add r4 r2 r3
st r5 r4 0
add r7 r5 r6
addi r5 r6 0
addi r6 r7 0
addi r3 r3 1
j -9

