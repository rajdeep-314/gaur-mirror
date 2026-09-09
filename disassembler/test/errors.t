*** Test disassembler with input file whose size is not a multiple of 4 bytes

  $ ocaml scripts/arbitary_binary.ml 5 > input.bin
  $ wc -c input.bin
  5 input.bin
  $ dis input.bin -o output.pasm &> /dev/null
  [1]
