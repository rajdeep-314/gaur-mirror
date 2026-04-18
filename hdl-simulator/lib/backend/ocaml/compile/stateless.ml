module E = Expressions
module F = Functions
module M = Modules
module N = Name
module P = Patterns
module T = Types

open Parse
open Hdlutils

module StrMap = Map.Make(String)

let rec typ (t : Types.t) : T.typ =
  match t with
  | TBits   n           -> (T.tid "bv")
  | TRecord tr          -> StrMap.map typ tr |> T.tobject
  | TChip   (tin, tout) -> T.tarrow (typ tin) (typ tout)

let rec expr (ei : 'a Ast.expr_info) : E.expr =
  match ei.data with
  | Const  bv           -> expr_const bv
  | Record r            -> StrMap.map expr r |> E.eobject
  | Splice (e', lb, ub) -> F.splice (expr e') (E.eint lb) (E.eint ub)
  | Concat (e1, e2)     -> F.concat (expr e1) (expr e1)
  | Access id           -> E.eid id
  | Member (e', fld)    -> E.eobjmem (expr e') fld

  and expr_const bv =
    NonEmpty.to_list bv
    |> List.map E.ebool
    |> E.earray

let rhs_out id e : E.expr =
  E.eapply (E.modpath id N.fun_out) [expr e]

let rhs_clk id e : E.expr =
  E.eapply (E.modpath id N.fun_clk) [expr e]

let rhs (ri : 'a Ast.rhs_info) : E.expr =
  match ri.data with
  | Call (id, e) -> rhs_out id e
  | Expr e       -> expr e

let assign (ai : 'a Ast.assign_info) : E.expr -> E.expr =
  match ai.data with
  | Current (id, r)
  | Next    (id, r) -> E.elet (P.pid id) (rhs r)

let rec parts (pi : 'a Ast.assign_info list) expr_last : E.expr =
  match pi with
  | []        -> expr_last
  | ai :: ais -> let e  = assign ai in
                 let es = parts ais expr_last in
                 e es


let tin_typ (ci : 'a Ast.chip_info) : T.typ =
  typ ci.data.input.typ

let tin_decl (ci : 'a Ast.chip_info) : T.typ_decl =
  T.decl_alias N.id_tin (tin_typ ci)

let tin_tid = T.tid N.id_tin


let tout_typ (ci : 'a Ast.chip_info) : T.typ =
  typ ci.data.output.typ

let tout_decl (ci : 'a Ast.chip_info) : T.typ_decl =
  T.decl_alias N.id_tout (tout_typ ci)

let tout_tid = T.tid N.id_tout


let expr_in (ci : 'a Ast.chip_info) e =
  let input = ci.data.input.name in
  let bl =
    E.bfun
      N.fun_out
      [P.pid input, Some tin_tid]
      (Some tout_tid)
  in bl e


let expr_out (ci : 'a Ast.chip_info) =
  E.eid ci.data.output.name


let union (m1 : 'a StrMap.t) (m2 : 'a StrMap.t) =
  StrMap.fold (fun k v acc -> StrMap.add k v acc) m1 m2

let chip (ci : 'a Ast.chip_info) =
  let ein   = expr_in ci in
  let eout  = expr_out ci in
  let eparts = parts ci.data.parts in
  let elast = eout in
  M.mdecl
    ci.data.name
    [ M.item_typ_decl (tin_decl ci)
    ; M.item_typ_decl (tout_decl ci)
    ; M.item_let_bind (ein (eparts elast))
    ]

let prog (p : 'a Ast.prog) =
  List.map chip p
