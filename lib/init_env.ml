open Asttypes
open Parsetree
open Utils

let initial : Env.t =
  let nand_in_record =
    StrMap.empty
    |> StrMap.add "a" (TBits 1)
    |> StrMap.add "b" (TBits 1)
  in

  let nand_typ = TChip (TRecord nand_in_record, TBits 1) in
  Env.extend "Nand" nand_typ Env.empty_env

let extend_chip env chip =
  let chip_typ = TChip (chip.input.port_type, chip.output.port_type) in
  Env.extend chip.name chip_typ env

let init_chip program =
  List.fold_left
    extend_chip
    initial
    program
