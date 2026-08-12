Tests for disassembler instruction translation.


*** All PISA instructions ***

  $ assembler example/all_instructions.pasm --format bin -o all_instructions.pmac
  Wrote: all_instructions.pmac

  $ dis all_instructions.pmac -o all_instructions-dis.pasm
  Wrote: all_instructions-dis.pasm

  $ cat all_instructions-dis.pasm
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
  beq r23 r24 5
  blt r25 r26 5
  jalr r27 r28 7
  apc r29 -100
  j 3
  li r30 1
  li r31 2
  j 0
