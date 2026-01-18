Tests for the assembler executable's command line flags.



*** Default format is bin ***

  $ assembler ../examples/fib.pasm -fmt bin -o manual
  Wrote: manual
  $ assembler ../examples/fib.pasm -o default
  Wrote: default
  $ diff manual default && echo "no difference"
  no difference



*** Default output file name tests ***

Format: bin
Assembling fib.pasm with format bin should result in fib.pmac

  $ assembler ../examples/fib.pasm -fmt bin
  Wrote: ../examples/fib.pmac
  $ [ -f ../examples/fib.pmac ] && echo "fib.pmac exists"
  fib.pmac exists


Format: inter
Assembling fib.pasm with format inter should result in fib.pasm.inter

  $ assembler ../examples/fib.pasm -fmt inter
  Wrote: ../examples/fib.pasm.inter
  $ [ -f ../examples/fib.pasm.inter ] && echo "fib.pasm.inter exists"
  fib.pasm.inter exists


Format: ascii
Assembling fib.pasm with format ascii should result in fib.pasc

  $ assembler ../examples/fib.pasm -fmt ascii
  Wrote: ../examples/fib.pasc
  $ [ -f ../examples/fib.pasc ] && echo "fib.pasc exists"
  fib.pasc exists

