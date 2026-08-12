# Test signed and unsigned immediate boundaries.

# li: unsigned [0, 2047]
li r1 0
li r2 2047

# ld/st/addi/jalr: signed [-32, 31]
ld r3 r4 -32
ld r5 r6 31

st r7 r8 -32
st r9 r10 31

addi r11 r12 -32
addi r13 r14 31

jalr r15 r16 -32
jalr r17 r18 31

# APC has a numeric immediate because it is not label-based.
apc r26 -1024
apc r27 1023
