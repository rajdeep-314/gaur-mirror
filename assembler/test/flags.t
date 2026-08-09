Tests for the assembler executable's command line flags.

*** Default format is bin ***

  $ assembler ../examples/fib.pasm -fmt bin -o manual
  Wrote: manual

  $ assembler ../examples/fib.pasm -o default
  Wrote: default

  $ diff manual default && echo "no difference"
  no difference

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

*** Default output file name: bin ***

  $ rm -f ../examples/fib.pmac
  $ assembler ../examples/fib.pasm -fmt bin
  Wrote: ../examples/fib.pmac

  $ [ -f ../examples/fib.pmac ] && echo "fib.pmac exists"
  fib.pmac exists

*** Default output file name: inter ***

  $ rm -f ../examples/fib.pasm.inter
  $ assembler ../examples/fib.pasm -fmt inter
  Wrote: ../examples/fib.pasm.inter

  $ [ -f ../examples/fib.pasm.inter ] && echo "fib.pasm.inter exists"
  fib.pasm.inter exists

*** Default output file name: ascii ***

  $ rm -f ../examples/fib.pasc
  $ assembler ../examples/fib.pasm -fmt ascii
  Wrote: ../examples/fib.pasc

  $ [ -f ../examples/fib.pasc ] && echo "fib.pasc exists"
  fib.pasc exists

*** Long help flag ***

  $ assembler --help | head -n 1
  Usage: assembler [options] <file_path>.
    --format 
  	Output format (bin | inter | ascii)
    -fmt 
  	Short for --format
    --out 
  	Output file path
    -o 
  	Short for --out
    --help 
  	Display this list of options
    -help 
  	Display this list of options
    -h 
  	Short for --help / -help

*** Short help flags ***

  $ assembler -h | head -n 1
  Usage: assembler [options] <file_path>.
    --format 
  	Output format (bin | inter | ascii)
    -fmt 
  	Short for --format
    --out 
  	Output file path
    -o 
  	Short for --out
    --help 
  	Display this list of options
    -help 
  	Display this list of options
    -h 
  	Short for --help / -help

  $ assembler -help | head -n 1
  Usage: assembler [options] <file_path>.
    --format 
  	Output format (bin | inter | ascii)
    -fmt 
  	Short for --format
    --out 
  	Output file path
    -o 
  	Short for --out
    --help 
  	Display this list of options
    -help 
  	Display this list of options
    -h 
  	Short for --help / -help

*** Missing input file ***

  $ assembler >/dev/null 2>&1
  [1]


*** Too many positional arguments ***

  $ assembler ../examples/fib.pasm ../examples/fib.pasm >/dev/null 2>&1
  [1]


*** Unknown output format ***

  $ assembler ../examples/fib.pasm --format xyz >/dev/null 2>&1
  [2]
