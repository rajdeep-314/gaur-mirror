Tests for jump and branch immediate generation.


*** Jump stress test ***

  $ bash example/jumps.sh
  Generated: example/jumps.pasm
  Instructions: 65686

  $ assembler example/jumps.pasm --format bin -o jumps.pmac
  Wrote: jumps.pmac

  $ dis jumps.pmac -o jumps-dis.pasm
  Wrote: jumps-dis.pasm
