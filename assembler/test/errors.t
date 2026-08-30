Tests for invalid PASM input.


*** Invalid register ***

  $ assembler ../examples/errors/invalid_register.pasm --format inter 2>/dev/null
  [2]


*** Missing operand ***

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

*** Jump overflow ***

  $ assembler ../examples/errors/jump_overflow.pasm --format inter &>/dev/null
  [3]

*** Backward jump overflow ***

  $ assembler ../examples/errors/backward_jump_overflow.pasm --format inter &>/dev/null
  [3]
