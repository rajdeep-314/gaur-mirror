module E = Expressions
module F = Functions
module M = Modules
module N = Name
module P = Patterns
module T = Types

module A = Parse.Ast

open Hdlutils

let select_match_clause s =
  P.pid s, E.evar "Some" [E.emodule s]

let select_match (p : 'a A.prog) : E.expr =
  p
  |> List.map A.Mnemonics.chip_info_name
  |> List.map select_match_clause
  |> List.append [ P.pwild, E.eid "None" ]
  |> E.ematch (E.eid "name")

let select_fun p =
  E.bfun
    "select"
    [ P.pid "name", None ]
    (Some (T.tmodsig "CHIP"))
    (select_match p)

let select p =
  M.mdecl "Chips" [
    M.item_let_bind (select_fun p)
  ]
