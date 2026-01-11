type err = 
  | InvalidOpcode

let print_err = function
  | InvalidOpcode -> "Error: Invalid Opcode"