*** These test cases test things related to the executable's command-line arguments.
***
*** List of things currently being tested:
*** - The default flags are as expected.
*** - Long and short forms of the flags are equivalent.
*** - Missing or extra arguments.
*** - Invalid flag value.


*** Default flag tests ***


*** Default format is bin ***

  $ assembler ../examples/fib.pasm --format bin --out manual
  Wrote: manual

  $ assembler ../examples/fib.pasm --out default
  Wrote: default

  $ diff manual default && echo "no difference"
  no difference


*** Default output file name for format bin ***

  $ assembler ../examples/fib.pasm -fmt bin
  Wrote: ../examples/fib.pmac

  $ [ -f ../examples/fib.pmac ] && echo "fib.pmac exists"
  fib.pmac exists


*** Default output file name for format inter ***

  $ assembler ../examples/fib.pasm -fmt inter
  Wrote: ../examples/fib.pasm.inter

  $ [ -f ../examples/fib.pasm.inter ] && echo "fib.pasm.inter exists"
  fib.pasm.inter exists


*** Default output file name for format ascii ***

  $ assembler ../examples/fib.pasm -fmt ascii
  Wrote: ../examples/fib.pasc

  $ [ -f ../examples/fib.pasc ] && echo "fib.pasc exists"
  fib.pasc exists



*** Long and short format flags ***


*** Long and short format flags ***

  $ assembler ../examples/fib.pasm --format inter -o long-format
  Wrote: long-format

  $ assembler ../examples/fib.pasm -fmt inter -o short-format
  Wrote: short-format

  $ diff long-format short-format && echo "no difference"
  no difference


*** Long and short output flags ***

  $ assembler ../examples/fib.pasm --format inter --out long-out
  Wrote: long-out

  $ assembler ../examples/fib.pasm -fmt inter -o short-out
  Wrote: short-out

  $ diff long-out short-out && echo "no difference"
  no difference


*** Long and short help flags ***

  $ assembler --help 2> assembler-long-help-out

  $ assembler -h 2> assembler-short-help-out

  $ assembler -help 2> assembler-short-help-2-out

  $ diff3 assembler-long-help-out assembler-short-help-out assembler-short-help-2-out && echo "no difference"
  no difference



*** Missing or extra arguments ***


*** Missing input file ***

  $ assembler >/dev/null 2>&1
  [1]


*** Too many positional arguments ***

  $ assembler ../examples/fib.pasm ../examples/fib.pasm >/dev/null 2>&1
  [1]



*** Invalid flag value ***


*** Unknown format flag value ***

  $ assembler ../examples/fib.pasm --format xyz >/dev/null 2>&1
  [2]
