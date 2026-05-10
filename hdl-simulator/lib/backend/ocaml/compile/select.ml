module E = Expressions
module F = Functions
module M = Modules
module N = Name
module P = Patterns
module T = Types

module A = Parse.Ast

open Hdlutils

let select_match_clause s =
  P.pstring s, E.evar "Some" [E.emodule s]

let select_match (p : 'a A.prog) : E.expr =
  let match_clauses =
    p
    |> List.map A.Mnemonics.chip_info_name
    |> List.map select_match_clause
  in
  let match_wild = [ P.pwild, E.eid "None" ] in
  E.ematch
    (E.eid "name")
    (match_clauses @ match_wild)

let select_fun p =
  E.bfun
    "select"
    [ P.pid "name", None ]
    (Some (T.tpoly [T.tmodsig "CHIP"] (T.tid "option")))
    (select_match p)

let select p =
  M.mdecl "Chips" [
    M.item_let_bind (select_fun p)
  ]
