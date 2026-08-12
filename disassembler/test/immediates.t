Tests for disassembler immediate decoding and sign extension.


*** Immediate boundaries ***

  $ assembler example/immediate_boundaries.pasm --format bin -o immediate_boundaries.pmac
  Wrote: immediate_boundaries.pmac

  $ dis immediate_boundaries.pmac -o immediate_boundaries-dis.pasm
  Wrote: immediate_boundaries-dis.pasm

  $ cat immediate_boundaries-dis.pasm
  li r1 0
  li r2 2047
  ld r3 r4 -32
  ld r5 r6 31
  st r7 r8 -32
  st r9 r10 31
  addi r11 r12 -32
  addi r13 r14 31
  jalr r15 r16 -32
  jalr r17 r18 31
  apc r26 -1024
  apc r27 1023
