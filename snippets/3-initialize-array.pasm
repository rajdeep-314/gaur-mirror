# 3. Array initialization.
# Set RAM[1] to RAM[25] to 0xc001.


lui r1 0
lli r1 5
lui r2 0xc0
lli r2 0x01
addi r3 r0 1

blt r1 r3 4
st r2 r3 0
addi r3 r3 1
j -3

