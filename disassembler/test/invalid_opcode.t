Tests that the disassembler rejects invalid opcodes.


*** Opcode 0x0000 ***

  $ bash binaries/invalid_opcode.sh >/dev/null
  $ dis binaries/invalid_opcode_0000.pmac -o invalid_opcode_0000.pasm
  Invalid Opcode Error:
    Inst   : 0x00000000
    Offset : 0
    Opcode : 0x0000
  [2]


*** Opcode 0x0003 ***

  $ dis binaries/invalid_opcode_0003.pmac -o invalid_opcode_0003.pasm
  Invalid Opcode Error:
    Inst   : 0x00030000
    Offset : 0
    Opcode : 0x0003
  [2]


*** Opcode 0x0005 ***

  $ dis binaries/invalid_opcode_0005.pmac -o invalid_opcode_0005.pasm
  Invalid Opcode Error:
    Inst   : 0x00050000
    Offset : 0
    Opcode : 0x0005
  [2]


*** Opcode 0x000f ***

  $ dis binaries/invalid_opcode_000f.pmac -o invalid_opcode_000f.pasm
  Invalid Opcode Error:
    Inst   : 0x000F0000
    Offset : 0
    Opcode : 0x000F
  [2]


*** Opcode 0x00ff ***

  $ dis binaries/invalid_opcode_00ff.pmac -o invalid_opcode_00ff.pasm
  Invalid Opcode Error:
    Inst   : 0x00FF0000
    Offset : 0
    Opcode : 0x00FF
  [2]


*** Opcode 0x0300 ***

  $ dis binaries/invalid_opcode_0300.pmac -o invalid_opcode_0300.pasm
  Invalid Opcode Error:
    Inst   : 0x03000000
    Offset : 0
    Opcode : 0x0300
  [2]


*** Opcode 0x0fff ***

  $ dis binaries/invalid_opcode_0fff.pmac -o invalid_opcode_0fff.pasm
  Invalid Opcode Error:
    Inst   : 0x0FFF0000
    Offset : 0
    Opcode : 0x0FFF
  [2]


*** Opcode 0x3000 ***

  $ dis binaries/invalid_opcode_3000.pmac -o invalid_opcode_3000.pasm
  Invalid Opcode Error:
    Inst   : 0x30000000
    Offset : 0
    Opcode : 0x3000
  [2]


*** Opcode 0x7fff ***

  $ dis binaries/invalid_opcode_7fff.pmac -o invalid_opcode_7fff.pasm
  Invalid Opcode Error:
    Inst   : 0x7FFF0000
    Offset : 0
    Opcode : 0x7FFF
  [2]


*** Opcode 0x8000 ***

  $ dis binaries/invalid_opcode_8000.pmac -o invalid_opcode_8000.pasm
  Invalid Opcode Error:
    Inst   : 0x80000000
    Offset : 0
    Opcode : 0x8000
  [2]


*** Opcode 0xffff ***

  $ dis binaries/invalid_opcode_ffff.pmac -o invalid_opcode_ffff.pasm
  Invalid Opcode Error:
    Inst   : 0xFFFF0000
    Offset : 0
    Opcode : 0xFFFF
  [2]
