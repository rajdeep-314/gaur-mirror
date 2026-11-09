open Errors

module StrMap = Map.Make(String)

let rec tc_expr (ei : 'a Parse.Ast.expr_info) : Parse.Types.typ =
  match ei.expr with
  | Const bv   -> let l = Array.length bv in
                  TBits l
  | Record er  -> let tr = StrMap.map tc_expr er in
                  Parse.Types.TRecord tr

let tc_input typeof_tin (ip : Parse.Loc.t Parse.Ast.expr_info) =
  let loc = ip.info in
  let ta = tc_expr ip in
  let te = typeof_tin in
  if Parse.Types.eq te ta
  then ()
  else raise_mismatch loc ta te
