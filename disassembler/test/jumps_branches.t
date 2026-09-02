*** These test cases test jump and branch instruction encoding and decoding. ***

*** - Forward and backward BEQ offsets ***
*** - Forward and backward BLT offsets ***
*** - Forward and backward J offsets ***
*** - Maximum positive and negative PC-relative offsets ***
*** - Assembler/disassembler round-trip correctness ***


*** Maximum forward BEQ offset (+32) ***

  $ bash scripts/jumps.sh forward beq 31 beq-forward-max.pasm
  $ assembler beq-forward-max.pasm --format bin -o beq-forward-max.pmac
  Wrote: beq-forward-max.pmac
  $ dis beq-forward-max.pmac -o beq-forward-max-dis.pasm
  Wrote: beq-forward-max-dis.pasm


*** Maximum backward BEQ offset (-32) ***

  $ bash scripts/jumps.sh backward beq 32 beq-backward-max.pasm
  $ assembler beq-backward-max.pasm --format bin -o beq-backward-max.pmac
  Wrote: beq-backward-max.pmac
  $ dis beq-backward-max.pmac -o beq-backward-max-dis.pasm
  Wrote: beq-backward-max-dis.pasm


*** Maximum forward BLT offset (+31) ***

  $ bash scripts/jumps.sh forward blt 31 blt-forward-max.pasm
  $ assembler blt-forward-max.pasm --format bin -o blt-forward-max.pmac
  Wrote: blt-forward-max.pmac
  $ dis blt-forward-max.pmac -o blt-forward-max-dis.pasm
  Wrote: blt-forward-max-dis.pasm


*** Maximum backward BLT offset (-32) ***

***  $ bash scripts/jumps.sh backward blt 32 blt-backward-max.pasm
***  $ assembler blt-backward-max.pasm --format bin -o blt-backward-max.pmac
***  Wrote: blt-backward-max.pmac
***  $ dis blt-backward-max.pmac -o blt-backward-max-dis.pasm
***  Wrote: blt-backward-max-dis.pasm


*** Maximum forward J offset (+32767) ***

  $ bash scripts/jumps.sh forward j 32767 j-forward-max.pasm
  $ assembler j-forward-max.pasm --format bin -o j-forward-max.pmac
  Wrote: j-forward-max.pmac
  $ dis j-forward-max.pmac -o j-forward-max-dis.pasm
  Wrote: j-forward-max-dis.pasm


*** Maximum backward J offset (-32768) ***

  $ bash scripts/jumps.sh backward j 32768 j-backward-max.pasm
  $ assembler j-backward-max.pasm --format bin -o j-backward-max.pmac
  Wrote: j-backward-max.pmac
  $ dis j-backward-max.pmac -o j-backward-max-dis.pasm
  Wrote: j-backward-max-dis.pasm
