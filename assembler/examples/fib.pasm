# fib.pasm

# Let RAM[0] store n and RAM[1] store k.
# This program computes the first n Fibonacci numbers and puts them
# in RAM, starting at address k.


ld r1 r0 0
ld r2 r0 1

addi r3 r0 0
addi r5 r0 0
addi r6 r0 1

loop:
    beq  r3 r1 halt
    add  r4 r2 r3
    st   r5 r4 0
    add  r6 r6 r5
    sub  r5 r6 r5
    addi r3 r3 1
    j loop

halt:
    j halt

