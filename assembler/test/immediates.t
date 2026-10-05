*** These test cases test the minimum and maximum valid immediate values. ***

*** - li immediate boundaries*
*** - ld immediate boundaries*
*** - st immediate boundaries*
*** - addi immediate boundaries*
*** - jalr immediate boundaries*
*** - apc immediate boundaries*

*** li immediate boundaries ***

  $ ocaml scripts/immediate.ml li 0 > li-min.gsm
  $ assembler li-min.gsm --format ascii -o li-min-ascii >/dev/null
  $ cat li-min-ascii
  00000000000000010000100000000000

  $ ocaml scripts/immediate.ml li 2047 > li-max.gsm
  $ assembler li-max.gsm --format ascii -o li-max-ascii >/dev/null
  $ cat li-max-ascii
  00000000000000010000111111111111


*** ld immediate boundaries ***

  $ ocaml scripts/immediate.ml ld -32 > ld-min.gsm
  $ assembler ld-min.gsm --format ascii -o ld-min-ascii >/dev/null
  $ cat ld-min-ascii
  00000000000000100000100010100000

  $ ocaml scripts/immediate.ml ld 31 > ld-max.gsm
  $ assembler ld-max.gsm --format ascii -o ld-max-ascii >/dev/null
  $ cat ld-max-ascii
  00000000000000100000100010011111


*** st immediate boundaries ***

  $ ocaml scripts/immediate.ml st -32 > st-min.gsm
  $ assembler st-min.gsm --format ascii -o st-min-ascii >/dev/null
  $ cat st-min-ascii
  00000000000001000000100010100000

  $ ocaml scripts/immediate.ml st 31 > st-max.gsm
  $ assembler st-max.gsm --format ascii -o st-max-ascii >/dev/null
  $ cat st-max-ascii
  00000000000001000000100010011111


*** addi immediate boundaries ***

  $ ocaml scripts/immediate.ml addi -32 > addi-min.gsm
  $ assembler addi-min.gsm --format ascii -o addi-min-ascii >/dev/null
  $ cat addi-min-ascii
  00000000000100000000100010100000

  $ ocaml scripts/immediate.ml addi 31 > addi-max.gsm
  $ assembler addi-max.gsm --format ascii -o addi-max-ascii >/dev/null
  $ cat addi-max-ascii
  00000000000100000000100010011111


*** jalr immediate boundaries ***

  $ ocaml scripts/immediate.ml jalr -32 > jalr-min.gsm
  $ assembler jalr-min.gsm --format ascii -o jalr-min-ascii >/dev/null
  $ cat jalr-min-ascii
  00010000000000000000100010100000

  $ ocaml scripts/immediate.ml jalr 31 > jalr-max.gsm
  $ assembler jalr-max.gsm --format ascii -o jalr-max-ascii >/dev/null
  $ cat jalr-max-ascii
  00010000000000000000100010011111


*** apc immediate boundaries ***

  $ ocaml scripts/immediate.ml apc -1024 > apc-min.gsm
  $ assembler apc-min.gsm --format ascii -o apc-min-ascii >/dev/null
  $ cat apc-min-ascii
  00100000000000000000110000000000

  $ ocaml scripts/immediate.ml apc 1023 > apc-max.gsm
  $ assembler apc-max.gsm --format ascii -o apc-max-ascii >/dev/null
  $ cat apc-max-ascii
  00100000000000000000101111111111
