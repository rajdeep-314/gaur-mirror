*** These test cases test jump and branch immediate generation.

*** - All the test cases are generated to single jumps.pasm file by jumps.sh
*** - Forward and backward BEQ offsets
*** - Forward and backward BLT offsets
*** - Forward and backward jump offsets
*** - Generation of immediate values for short and long PC-relative jumps
*** - Correct handling of large positive and negative jump offsets


*** Jump stress test ***

  $ bash example/jumps.sh
  Generated: example/jumps.pasm

  $ assembler example/jumps.pasm --format bin -o jumps.pmac
  Wrote: jumps.pmac

  $ dis jumps.pmac -o jumps-dis.pasm
  Wrote: jumps-dis.pasm
