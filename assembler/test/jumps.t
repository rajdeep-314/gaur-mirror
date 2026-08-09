Tests for PASM jump and label translation.


*** Forward unconditional and conditional jumps ***

  $ assembler ../examples/jumps/forward.pasm --format ascii -o forward-ascii
  Wrote: forward-ascii
  $ cat forward-ascii
  00000000000000010000100000001010
  00000100000000000000100001000010
  00000000000100000001000010000001
  00000000000000010001100000010100
  01000000000000000000000000000001
  01000000000000000000000000000000


*** Backward unconditional jumps ***

  $ assembler ../examples/jumps/backward.pasm --format ascii -o backward-ascii
  Wrote: backward-ascii
  $ cat backward-ascii
  00000000000000010000100000001010
  00000000000100000000100001111111
  01000000000000001111111111111110


*** Forward and backward conditional jumps ***

  $ assembler ../examples/jumps/conditional.pasm --format ascii -o conditional-ascii
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

  $ assembler ../examples/jumps/zero_offset.pasm --format ascii -o zero_offset-ascii
  Wrote: zero_offset-ascii
  $ cat zero_offset-ascii
  01000000000000000000000000000000
