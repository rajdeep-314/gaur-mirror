*** These test cases test that assembling and then disassembling a valid PASM program produces an equivalent PASM program.

*** - Round-trip correctness of the assembler and disassembler
*** - Consistency between disassembled output and the intermediate representation
*** - Preservation of all valid instructions during assembly and disassembly

  $ assembler example/all_instructions.pasm -fmt bin -o asm_binary.pmac
  Wrote: asm_binary.pmac

  $ dis asm_binary.pmac -o dis.pasm
  Wrote: dis.pasm

  $ assembler example/all_instructions.pasm -fmt inter -o inter.pasm
  Wrote: inter.pasm

  $ cmp dis.pasm inter.pasm && echo "invertability successful"
  invertability successful
