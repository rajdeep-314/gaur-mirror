# Test program containing every PASM/PISA instruction.
# Jump and branch instructions use labels.

li r1 123

ld r2 r3 -12

st r4 r5 20

add r6 r7 r8

addi r9 r10 -15

sub r11 r12 r13

and r14 r15 r16

not r17 r18

sl r19 r20 5

sr r21 r22 3

# Branch instructions use labels.
beq r23 r24 equal_target
blt r25 r26 less_target

# Register-based jump.
jalr r27 r28 7

# PC-relative instruction.
apc r29 -100

# Unconditional jump.
j jump_target

equal_target:
li r30 1

less_target:
li r31 2

jump_target:
spin