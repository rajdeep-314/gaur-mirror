*** Test disassembler with input file whose size is not a multiple of 4 bytes

  $ ocaml scripts/arbitary_binary.ml 5 > input.bram
  $ wc -c input.bram
  5 input.bram
  $ cpusim snippets/fib.pasm.inter -r input.bram -ms=69 -fs -nt &> /dev/null
  [1]
