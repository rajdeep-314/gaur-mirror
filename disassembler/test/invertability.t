Tests that assembling and then disassembling a valid PASM program
produces an equivalent PASM program.

*** Assembler -> Disassembler ***
*** Assembler -> Intermediate Representation ***

  $ assembler example/all_instructions.pasm -fmt bin -o asm_binary.pmac
  Wrote: asm_binary.pmac

  $ dis asm_binary.pmac -o dis.pasm
  Wrote: dis.pasm

  $ assembler example/all_instructions.pasm -fmt inter -o inter.pasm
  Wrote: inter.pasm

  $ cmp dis.pasm inter.pasm && echo "invertability successful"
  invertability successful
