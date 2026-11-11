*** These test cases check that out-of-range immediate values are rejected. ***

*** li ***

  $ assembler ../examples/oob_instruction_immediates/li_lb.gsm --format inter >/dev/null 2>&1
  [2]
  $ assembler ../examples/oob_instruction_immediates/li_ub.gsm --format inter >/dev/null 2>&1
  [2]

*** ld ***

  $ assembler ../examples/oob_instruction_immediates/ld_lb.gsm --format inter >/dev/null 2>&1
  [2]
  $ assembler ../examples/oob_instruction_immediates/ld_ub.gsm --format inter >/dev/null 2>&1
  [2]

*** st ***

  $ assembler ../examples/oob_instruction_immediates/st_lb.gsm --format inter >/dev/null 2>&1
  [2]
  $ assembler ../examples/oob_instruction_immediates/st_ub.gsm --format inter >/dev/null 2>&1
  [2]

*** addi ***

  $ assembler ../examples/oob_instruction_immediates/addi_lb.gsm --format inter >/dev/null 2>&1
  [2]
  $ assembler ../examples/oob_instruction_immediates/addi_ub.gsm --format inter >/dev/null 2>&1
  [2]

*** sl ***

  $ assembler ../examples/oob_instruction_immediates/sl_lb.gsm --format inter >/dev/null 2>&1
  [2]
  $ assembler ../examples/oob_instruction_immediates/sl_ub.gsm --format inter >/dev/null 2>&1
  [2]

*** sr ***

  $ assembler ../examples/oob_instruction_immediates/sr_lb.gsm --format inter >/dev/null 2>&1
  [2]
  $ assembler ../examples/oob_instruction_immediates/sr_ub.gsm --format inter >/dev/null 2>&1
  [2]

*** jalr ***

  $ assembler ../examples/oob_instruction_immediates/jalr_lb.gsm --format inter >/dev/null 2>&1
  [2]
  $ assembler ../examples/oob_instruction_immediates/jalr_ub.gsm --format inter >/dev/null 2>&1
  [2]

*** apc ***

  $ assembler ../examples/oob_instruction_immediates/apc_lb.gsm --format inter >/dev/null 2>&1
  [2]
  $ assembler ../examples/oob_instruction_immediates/apc_ub.gsm --format inter >/dev/null 2>&1
  [2]
