# 5. Sorting.
# Let the base address and length of an array of half words be present in
# RAM[0] and RAM[1], respectively. Sort the array in a non-decreasing order,
# in-place.


ld r1 r0 0
ld r2 r0 1
addi r3 r0 0
addi r2 r2 -1

beq r3 r2 2
j 2
j 12
add r4 r1 r3
ld r5 r4 0
ld r6 r4 1
blt r5 r6 6
st r6 r4 0
st r5 r4 1
addi r6 r7 0
addi r3 r0 0
j -11
addi r3 r3 1
j -13

