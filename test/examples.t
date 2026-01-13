Tests for the example programs.


*** Binary ***

  $ assembler ../examples/fib.pasm -fmt bin -o fib-bin
  Wrote: fib-bin
  $ xxd -g 1 fib-bin
  00000000: 21 00 22 01 53 00 55 00 56 01 93 17 44 23 35 40  !.".S.U.V...D#5@
  00000010: 46 65 65 65 53 31 df fa d0 00                    FeeeS1....


*** Intermediate ***

  $ assembler ../examples/fib.pasm -fmt inter -o fib-inter
  Wrote: fib-inter
  $ cat fib-inter
  ld r1 r0 0
  ld r2 r0 1
  addi r3 r0 0
  addi r5 r0 0
  addi r6 r0 1
  beq r3 r1 7
  add r4 r2 r3
  st r5 r4 0
  add r6 r6 r5
  sub r5 r6 r5
  addi r3 r3 1
  j -6
  j 0


*** ASCII ***

  $ assembler ../examples/fib.pasm -fmt ascii -o fib-ascii
  Wrote: fib-ascii
  $ cat fib-ascii
  0010000100000000
  0010001000000001
  0101001100000000
  0101010100000000
  0101011000000001
  1001001100010111
  0100010000100011
  0011010101000000
  0100011001100101
  0110010101100101
  0101001100110001
  1101111111111010
  1101000000000000

