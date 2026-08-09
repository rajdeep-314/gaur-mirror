# all_inst.pasm

# All 16 instructions are used for test.

li r1 10
ld r2 r1 4
st r2 r1 4
add r3 r1 r2
addi r4 r3 5
sub r5 r4 r1
and r6 r1 r2
not r7 r6
sl r8 r7 2
sr r9 r8 1
beq r1 r2 target
blt r2 r3 target
jalr r10 r1 4
apc r11 8
j target
spin

target: