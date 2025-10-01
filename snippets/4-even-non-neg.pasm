# 4. Counting even and non-negative integers.
# Let the base address and length of an array of half words be present in
# RAM[1] and RAM[0], respectively. Count the number of non-negative, even numbers
# in the array and store the count in r10.


ld r1 r0 0
ld r2 r0 1
addi r10 r0 0
addi r3 r0 0
addi r5 r0 1

beq r3 r1 2
j 2
j 9
add r4 r2 r3
ld r4 r4 0
blt r4 r0 4
and_ r4 r4 r5
beq r4 r5 2
addi r10 r10 1

addi r3 r3 1
j -10

