Testing output on some sample PISA binaries.


*** fib-out.pmac ***

  $ dis binaries/fib-out.pmac -o fib.pasm
  Wrote: fib.pasm
  $ cat fib.pasm
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

