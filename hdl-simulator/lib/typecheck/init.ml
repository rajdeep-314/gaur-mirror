open Parse

module StrMap = Map.Make(String)

let nand_in =
  let open Types in
  TRecord (
    StrMap.of_list
      [ ("a", TBits 1)
      ; ("b", TBits 1)
      ]
  )

let nand_out = Types.TBits 1

let nand_typ = Types.TChip (nand_in, nand_out)

let nand_env = Env.singleton "Nand" nand_typ

let add env (ci : 'a Ast.chip_info) =
  let c = ci.data in
  Env.add c.name (Ast.chip_typ c) env

let v prog = List.fold_left add nand_env prog
