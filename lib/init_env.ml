open Ast
open Asttypes

let initial : Env.t =
  let nand_in_record =
    StrMap.empty
    |> StrMap.add "a" (TBits 1)
    |> StrMap.add "b" (TBits 1)
  in

  let nand_typ = TChip (TRecord nand_in_record, TBits 1) in
  let nand_info =
    { typ=nand_typ;
      category=Chip;
      used=Unused;
      bound=Bound;
      updated=Unupdated
    }
  in

  Env.empty_env
  |> Env.extend "Nand" nand_info

let extend_chip env chip =
  let chip_typ = TChip (chip.input.port_type, chip.output.port_type) in
  let chip_info =
    { typ=chip_typ;
      category=Chip;
      used=Unused;
      bound=Bound;
      updated=Unupdated
    }
  in

  env
  |> Env.extend chip.name chip_info

let init_chip program =
  List.fold_left
    extend_chip
    initial
    program
