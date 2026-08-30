*** These test cases test if errors of the following kind are being caught:
***
*** - Syntax errors (lexing and parsing), such as:
***   - Invalid register
***   - Missing immediate operand
***   - Invalid instruction name
***   - Invalid character
***   - Missing register operand
***   - Tabs in a file
***   - Extra operand
*** - Assembly errors, namely:
***   - Missing jump target
***   - Missing beq target
***   - Missing blt target
***   - Forward jump overflow
***   - Backward jump overflow


*** Invalid register ***

  $ assembler ../examples/errors/invalid_register.pasm --format inter 2>/dev/null
  [2]


*** Missing immediate operand ***

  $ assembler ../examples/errors/missing_operand.pasm --format inter 2>/dev/null
  [2]


*** Invalid instruction ***

  $ assembler ../examples/errors/invalid_instruction.pasm --format inter 2>/dev/null
  [2]


*** Invalid character ***

  $ assembler ../examples/errors/invalid_character.pasm --format inter 2>/dev/null
  [2]


*** Missing register ***

  $ assembler ../examples/errors/missing_register.pasm --format inter 2>/dev/null
  [2]


*** Tabs in file ***

  $ assembler ../examples/errors/tabs.pasm --format inter 2>/dev/null
  [2]


*** Extra operand ***

  $ assembler ../examples/errors/extra_operand.pasm --format inter 2>/dev/null
  [2]


*** Missing jump target ***

  $ assembler ../examples/errors/missing_jump_target.pasm --format inter 2>/dev/null
  [2]


*** Missing beq target ***

  $ assembler ../examples/errors/missing_beq_target.pasm --format inter 2>/dev/null
  [3]


*** Missing blt target ***

  $ assembler ../examples/errors/missing_blt_target.pasm --format inter 2>/dev/null
  [3]


*** Forward jump overflow ***

  $ assembler ../examples/errors/forward_jump_overflow.pasm --format inter &>/dev/null
  [3]


*** Backward jump overflow ***

  $ assembler ../examples/errors/backward_jump_overflow.pasm --format inter &>/dev/null
  [3]
