*** These test cases test that the example programs assemble correctly.
***
*** List of current example programs being tested:
*** - fib.gsm (Fibonacci numbers)


*** Binary ***

  $ assembler ../examples/fib.gsm -fmt bin -o fib-bin
  Wrote: fib-bin
  $ xxd -g 1 fib-bin
  00000000: 00 02 08 00 00 02 10 01 00 10 18 00 00 10 28 00  ..............(.
  00000010: 00 10 30 01 04 00 18 47 00 08 20 86 00 04 29 00  ..0....G.. ...).
  00000020: 00 08 31 8a 00 20 29 8a 00 10 18 c1 40 00 ff fa  ..1.. ).....@...
  00000030: 40 00 00 00                                      @...


*** Intermediate ***

  $ assembler ../examples/fib.gsm -fmt inter -o fib-inter
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

  $ assembler ../examples/fib.gsm -fmt ascii -o fib-ascii
  Wrote: fib-ascii
  $ cat fib-ascii
  00000000000000100000100000000000
  00000000000000100001000000000001
  00000000000100000001100000000000
  00000000000100000010100000000000
  00000000000100000011000000000001
  00000100000000000001100001000111
  00000000000010000010000010000110
  00000000000001000010100100000000
  00000000000010000011000110001010
  00000000001000000010100110001010
  00000000000100000001100011000001
  01000000000000001111111111111010
  01000000000000000000000000000000

