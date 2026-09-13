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
***   - Duplicate label target
***   - Forward beq overflow
***   - Backward beq overflow
***   - Forward blt overflow
***   - Backward blt overflow
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


*** Tabs in non-comment lines ***

  $ assembler ../examples/errors/tabs.pasm --format inter 2>/dev/null
  [2]

*** Tabs in comments ***

  $ assembler ../examples/errors/tabs_in_comments.pasm --format inter 2>/dev/null
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

*** Duplicate Label target ***
  $ assembler ../examples/errors/dup_label.pasm --format inter 2>/dev/null
  [3]

*** Forward beq overflow ***
  $ ocaml scripts/forward.ml beq 32 > forward_beq_overflow.pasm
  $ assembler forward_beq_overflow.pasm --format inter &>/dev/null
  [3]


*** Backward beq overflow ***
  $ ocaml scripts/backward.ml beq 33 > backward_beq_overflow.pasm
  $ assembler backward_beq_overflow.pasm --format inter &>/dev/null
  [3]

*** Forward blt overflow ***

  $ ocaml scripts/forward.ml blt 32 > forward_blt_overflow.pasm
  $ assembler forward_blt_overflow.pasm --format inter &>/dev/null
  [3]


*** Backward blt overflow ***

  $ ocaml scripts/backward.ml blt 33 > backward_blt_overflow.pasm
  $ assembler backward_blt_overflow.pasm --format inter &>/dev/null
  [3]

*** Forward jump overflow ***

  $ ocaml scripts/forward.ml j 32768 > forward_jump_overflow.pasm
  $ assembler forward_jump_overflow.pasm --format inter &>/dev/null
  [3]


*** Backward jump overflow ***

  $ ocaml scripts/backward.ml j 32769 >  backward_jump_overflow.pasm
  $ assembler backward_jump_overflow.pasm --format inter &>/dev/null
  [3]
