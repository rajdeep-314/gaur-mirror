*** These test cases test jump and branch instruction encoding and decoding. ***

*** - Forward and backward BEQ offsets ***
*** - Forward and backward BLT offsets ***
*** - Forward and backward J offsets ***
*** - Maximum positive and negative PC-relative offsets ***
*** - Assembler/disassembler round-trip correctness ***


*** Maximum forward BEQ offset ***

  $ ocaml scripts/forward.ml beq 31 > beq-forward-max.pasm
  $ assembler beq-forward-max.pasm --format bin -o beq-forward-max.pmac >/dev/null
  $ dis beq-forward-max.pmac -o beq-forward-max-dis.pasm >/dev/null
  $ echo $?
  0



*** Maximum backward BEQ offset ***

  $ ocaml scripts/backward.ml beq 32 > beq-backward-max.pasm
  $ assembler beq-backward-max.pasm --format bin -o beq-backward-max.pmac >/dev/null
  $ dis beq-backward-max.pmac -o beq-backward-max-dis.pasm >/dev/null
  $ echo $?
  0


*** Maximum forward BLT offset ***

  $ ocaml scripts/forward.ml blt 31 > blt-forward-max.pasm
  $ assembler blt-forward-max.pasm --format bin -o blt-forward-max.pmac >/dev/null
  $ dis blt-forward-max.pmac -o blt-forward-max-dis.pasm >/dev/null
  $ echo $?
  0


*** Maximum backward BLT offset ***

  $ ocaml scripts/backward.ml blt 32 > blt-backward-max.pasm
  $ assembler blt-backward-max.pasm --format bin -o blt-backward-max.pmac >/dev/null
  $ dis blt-backward-max.pmac -o blt-backward-max-dis.pasm >/dev/null
  $ echo $?
  0



*** Maximum forward J offset ***

  $ ocaml scripts/forward.ml j 32767 > j-forward-max.pasm
  $ assembler j-forward-max.pasm --format bin -o j-forward-max.pmac >/dev/null
  $ dis j-forward-max.pmac -o j-forward-max-dis.pasm >/dev/null
  $ echo $?
  0


*** Maximum backward J offset ***

  $ ocaml scripts/backward.ml j 32768 > j-backward-max.pasm
  $ assembler j-backward-max.pasm --format bin -o j-backward-max.pmac >/dev/null
  $ dis j-backward-max.pmac -o j-backward-max-dis.pasm >/dev/null
  $ echo $?
  0
