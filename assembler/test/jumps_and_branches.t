*** These test cases test things related to jump and branch instructions.
*** List of things currently being tested: ***

*** - Normal forward and backward unconditional jumps work.
*** - Normal forward and backward conditional branches work.
*** - A zero-offset jump assembles as expected.
*** - Forward beq works with its maximum valid offset (+31).
*** - Backward beq works with its minimum valid offset (-32).
*** - Forward blt works with its maximum valid offset (+31).
*** - Backward blt works with its minimum valid offset (-32).
*** - Forward j works with its maximum valid offset (+32767).
*** - Backward j works with its minimum valid offset (-32768).


*** Forward unconditional and conditional jumps ***

  $ assembler ../examples/jumps_and_branches/forward.pasm --format ascii -o forward-ascii
  Wrote: forward-ascii
  $ cat forward-ascii
  00000000000000010000100000001010
  00000100000000000000100001000010
  00000000000100000001000010000001
  00000000000000010001100000010100
  01000000000000000000000000000001
  01000000000000000000000000000000


*** Backward unconditional jumps ***

  $ assembler ../examples/jumps_and_branches/backward.pasm --format ascii -o backward-ascii
  Wrote: backward-ascii
  $ cat backward-ascii
  00000000000000010000100000001010
  00000000000100000000100001111111
  01000000000000001111111111111110


*** Forward and backward conditional jumps ***

  $ assembler ../examples/jumps_and_branches/conditional.pasm --format ascii -o conditional-ascii
  Wrote: conditional-ascii
  $ cat conditional-ascii
  00000000000000010000100000001010
  00000000000000010001000000010100
  00000100000000000000100010000010
  00001000000000000000100010000001
  00000000000100000001100011000001
  00000100000000000001100011000001
  01000000000000001111111111111010


*** Zero-offset jump ***

  $ assembler ../examples/jumps_and_branches/zero_offset.pasm --format ascii -o zero_offset-ascii
  Wrote: zero_offset-ascii
  $ cat zero_offset-ascii
  01000000000000000000000000000000


*** Max beq forward jump ***

  $ ocaml scripts/forward.ml beq 31 > forward_beq_max.pasm
  $ assembler forward_beq_max.pasm --format ascii &>/dev/null

*** Min beq backward jump ***

  $ ocaml scripts/backward.ml beq 32 > backward_beq_max.pasm
  $ assembler backward_beq_max.pasm --format ascii &>/dev/null


*** Max blt forward jump ***

  $ ocaml scripts/forward.ml blt 31 > forward_blt_max.pasm
  $ assembler forward_blt_max.pasm --format ascii &>/dev/null


*** Min blt backward jump ***

  $ ocaml scripts/backward.ml blt 32 > backward_blt_max.pasm
  $ assembler backward_blt_max.pasm --format ascii &>/dev/null


*** Max j forward jump ***

  $ ocaml scripts/forward.ml j 32767  > forward_jump_max.pasm
  $ assembler forward_jump_max.pasm --format ascii &>/dev/null


*** Min j backward jump ***

  $ ocaml scripts/backward.ml j 32768 > backward_jump_max.pasm
  $ assembler backward_jump_max.pasm --format ascii &>/dev/null
