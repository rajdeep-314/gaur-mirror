*** These test cases test that the disassembler rejects instructions with invalid zero bits.

*** - Detection of non-zero reserved bits in ADD, SUB, AND, NOT, SL, and SR instructions
*** - Validation of instruction encoding constraints for required zero bits
*** - Correct reporting of invalid zero bit errors

*** ADD: bit 0 must be zero ***
  $ bash scripts/gen_bin.sh 0x0008 0x0001 > zero_bits_add.pmac
  $ dis zero_bits_add.pmac -o zero_bits_add.pasm
  Invalid Zero Bit Error:
    Inst     : 0x00080001
    Offset   : 0
    Expected : h<04>_rd(5)_rs1(5)_rs2(5)_0  : opcode 0x08 requires bit 0 to be zero
  [2]

*** SUB: bit 0 must be zero ***
  $ bash scripts/gen_bin.sh 0x0020 0x0001 > zero_bits_sub.pmac
  $ dis zero_bits_sub.pmac -o zero_bits_sub.pasm
  Invalid Zero Bit Error:
    Inst     : 0x00200001
    Offset   : 0
    Expected : h<06>_rd(5)_rs1(5)_rs2(5)_0  : opcode 0x20 requires bit 0 to be zero
  [2]

*** AND: bit 0 must be zero ***
  $ bash scripts/gen_bin.sh 0x0040 0x0001 > zero_bits_and.pmac
  $ dis zero_bits_and.pmac -o zero_bits_and.pasm
  Invalid Zero Bit Error:
    Inst     : 0x00400001
    Offset   : 0
    Expected : h<07>_rd(5)_rs1(5)_rs2(5)_0  : opcode 0x40 requires bit 0 to be zero
  [2]

*** NOT: bits [5:0] must be zero ***
  $ bash scripts/gen_bin.sh 0x0080 0x0001 > zero_bits_not.pmac
  $ dis zero_bits_not.pmac -o zero_bits_not.pasm
  Invalid Zero Bit Error:
    Inst     : 0x00800001
    Offset   : 0
    Expected : h<08>_rd(5)_rs1(5)_000000 : opcode 0x80 requires bits [5:0] must be zero
  [2]

*** SL: bit 5 must be zero ***
  $ bash scripts/gen_bin.sh 0x0100 0x0020 > zero_bits_sl.pmac
  $ dis zero_bits_sl.pmac -o zero_bits_sl.pasm
  Invalid Zero Bit Error:
    Inst     : 0x01000020
    Offset   : 0
    Expected : h<09>_rd(5)_rs(5)_0_imm(5)  : opcode 0x100 requires bit 5 to be zero
  [2]

*** SR: bit 5 must be zero ***
  $ bash scripts/gen_bin.sh 0x0200 0x0020 > zero_bits_sr.pmac
  $ dis zero_bits_sr.pmac -o zero_bits_sr.pasm
  Invalid Zero Bit Error:
    Inst     : 0x02000020
    Offset   : 0
    Expected : h<10>_rd(5)_rs(5)_0_imm(5)  : opcode 0x200 requires bit 5 to be zero
  [2]
