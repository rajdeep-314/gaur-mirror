*** These test cases test things related to the executable's command-line arguments.
***
*** List of things currently being tested:
*** - The default flags are as expected.
*** - Long and short forms of the flags are equivalent.
*** - Missing or extra arguments.
*** - Invalid flag value.


*** Default flag tests ***


*** Default format is bin ***

  $ assembler ../examples/fib.gsm --format bin --out manual
  Wrote: manual

  $ assembler ../examples/fib.gsm --out default
  Wrote: default

  $ diff manual default && echo "no difference"
  no difference


*** Default output file name for format bin ***

  $ assembler ../examples/fib.gsm -fmt bin
  Wrote: ../examples/fib.gin

  $ [ -f ../examples/fib.gin ] && echo "fib.gin exists"
  fib.gin exists


*** Default output file name for format inter ***

  $ assembler ../examples/fib.gsm -fmt inter
  Wrote: ../examples/fib.gsm.inter

  $ [ -f ../examples/fib.gsm.inter ] && echo "fib.gsm.inter exists"
  fib.gsm.inter exists


*** Default output file name for format ascii ***

  $ assembler ../examples/fib.gsm -fmt ascii
  Wrote: ../examples/fib.gsc

  $ [ -f ../examples/fib.gsc ] && echo "fib.gsc exists"
  fib.gsc exists



*** Long and short format flags ***


*** Long and short format flags ***

  $ assembler ../examples/fib.gsm --format inter -o long-format
  Wrote: long-format

  $ assembler ../examples/fib.gsm -fmt inter -o short-format
  Wrote: short-format

  $ diff long-format short-format && echo "no difference"
  no difference


*** Long and short output flags ***

  $ assembler ../examples/fib.gsm --format inter --out long-out
  Wrote: long-out

  $ assembler ../examples/fib.gsm -fmt inter -o short-out
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

  $ assembler ../examples/fib.gsm ../examples/fib.gsm >/dev/null 2>&1
  [1]



*** Invalid flag value ***


*** Unknown format flag value ***

  $ assembler ../examples/fib.gsm --format xyz >/dev/null 2>&1
  [1]
