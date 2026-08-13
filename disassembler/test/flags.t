*Tests for the disassembler executable's command line flags.*


*** Default output path ***

*The default output path should replace the input extension with .pasm.*

  $ rm -f binaries/fib-out.pasm
  $ dis binaries/fib-out.pmac
  Wrote: binaries/fib-out.pasm
  $ [ -f binaries/fib-out.pasm ] && echo "fib-out.pasm exists"
  fib-out.pasm exists


*** Short output flag (-o) ***

*The -o flag should write the disassembled program to the specified path.*

  $ dis binaries/fib-out.pmac -o binaries/fib-short.pasm
  Wrote: binaries/fib-short.pasm
  $ [ -f binaries/fib-short.pasm ] && echo "fib-short.pasm exists"
  fib-short.pasm exists


*** Long output flag (--out) ***

*The --out flag should write the disassembled program to the specified path.*

  $ dis binaries/fib-out.pmac --out binaries/fib-long.pasm
  Wrote: binaries/fib-long.pasm
  $ [ -f binaries/fib-long.pasm ] && echo "fib-long.pasm exists"
  fib-long.pasm exists


*** -o and --out produce the same output ***

*Both output flags should produce identical disassembly for the same input.*

  $ diff binaries/fib-short.pasm binaries/fib-long.pasm && echo "no difference"
  no difference


*** Help flag (-h) ***

*The -h flag should succeed without requiring an input file.*

  $ dis -h
  Usage: dis [options] <input_file_path>.
    --out 
  	Path for the generated PASM output
    -o 
  	Short for --out
    --help 
  	Display this list of options
    -help 
  	Display this list of options
    -h 
  	Short for --help / -help


*** Help flag (-help) ***

*The -help flag should succeed without requiring an input file.*

  $ dis -help
  Usage: dis [options] <input_file_path>.
    --out 
  	Path for the generated PASM output
    -o 
  	Short for --out
    --help 
  	Display this list of options
    -help 
  	Display this list of options
    -h 
  	Short for --help / -help


*** Help flag (--help) ***

*The --help flag should succeed without requiring an input file.*

  $ dis --help
  Usage: dis [options] <input_file_path>.
    --out 
  	Path for the generated PASM output
    -o 
  	Short for --out
    --help 
  	Display this list of options
    -help 
  	Display this list of options
    -h 
  	Short for --help / -help


*** Missing input file ***

*The disassembler should fail when no input file is provided.*

  $ dis >/dev/null 2>&1; echo $?
  1


*** Too many positional arguments ***

*The disassembler should reject more than one input file.*

  $ dis binaries/fib-out.pmac binaries/fib-out.pmac >/dev/null 2>&1; echo $?
  1


*** Invalid input file ***

*The disassembler should fail when the input file does not exist.*

  $ dis binaries/does-not-exist.pmac >/dev/null 2>&1; echo $?
  2
