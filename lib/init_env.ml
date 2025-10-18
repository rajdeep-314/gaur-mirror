open Ast

let initial =
  let nand_in_record = 
    StrMap.empty
    |> StrMap.add "a" (TBits 1)
    |> StrMap.add "b" (TBits 1)
  in
  
  Env.empty_env
  |> Env.extend "Nand" (TChip (TRecord nand_in_record, TBits 1))

let extend_chip env chip = 
  Env.extend
    chip.name
    (TChip (chip.input.port_type, chip.output.port_type))
    env

let init_chip program =
  List.fold_left
    extend_chip
    initial
    program
