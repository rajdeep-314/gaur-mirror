module E = Expressions
module F = Functions
module M = Modules
module N = Name
module P = Patterns
module T = Types
module D = Default
module Pr = Pretty

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
  | Concat (e1, e2)     -> F.concat (expr e1) (expr e2)
  | Access id           -> E.eid id
  | Member (e', fld)    -> E.eobjmem (expr e') fld

  and expr_const bv =
    NonEmpty.to_list bv
    |> List.map E.ebool
    |> E.earray

let rhs_out id e : E.expr =
  E.eapply
   (E.modpath id N.fun_output)
   [ expr e
   ; E.eunit
   ]

let rhs_clk id e : E.expr =
  E.eapply (E.modpath id N.fun_clock) [expr e]

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

let type_input_decl (ci : 'a Ast.chip_info) : T.typ_decl =
  T.decl_alias N.id_input (tin_typ ci)

let tin_tid = T.tid N.id_input


let tout_typ (ci : 'a Ast.chip_info) : T.typ =
  typ ci.data.output.typ

let type_output_decl (ci : 'a Ast.chip_info) : T.typ_decl =
  T.decl_alias N.id_output (tout_typ ci)

let tout_tid = T.tid N.id_output


let tchip_tid = T.tid N.id_state

let type_state_decl (ci : 'a Ast.chip_info) =
  T.decl_alias N.id_state T.tunit


let expr_output (ci : 'a Ast.chip_info) e =
  let input = ci.data.input.name in
  let bl =
    E.bfun
      N.fun_output
      [ P.pid input, Some tin_tid
      ; P.pwild, Some tchip_tid
      ]
      (Some tout_tid)
  in bl e


let expr_out (ci : 'a Ast.chip_info) =
  E.eid ci.data.output.name

let expr_clock (ci : 'a Ast.chip_info) =
  E.bfun
    N.fun_clock
    [ P.pwild, Some tin_tid
    ; P.pwild, Some tchip_tid
    ]
    (Some tchip_tid)
    E.eunit

let default_state =
  E.bfun N.fun_default_state [] (Some tchip_tid) E.eunit

let expr_default_input (ci : 'a Ast.chip_info) =
  E.bfun N.fun_default_input [] (Some (T.tid N.id_input)) (D.default_tin ci)

let union (m1 : 'a StrMap.t) (m2 : 'a StrMap.t) =
  StrMap.fold (fun k v acc -> StrMap.add k v acc) m1 m2

let chip (ci : 'a Ast.chip_info) =
  let default_input = expr_default_input ci in

  let output = expr_output ci in
  let eout  = expr_out ci in
  let eparts = parts ci.data.parts in
  let elast = eout in

  let clock = expr_clock ci in

  let typ_of_input = Type_of.typ_of_input ci in
  let input_of_expr = Of_expr.input_of_expr ci in
  let expr_of_output = To_expr.expr_of_output ci in

  M.mdecl
    ci.data.name
    [ M.item_typ_decl (type_state_decl ci)
    ; M.item_typ_decl (type_input_decl ci)
    ; M.item_typ_decl (type_output_decl ci)

    ; M.item_let_bind default_state
    ; M.item_let_bind default_input

    ; M.item_let_bind (output (eparts elast))
    ; M.item_let_bind clock

    ; M.item_let_bind typ_of_input
    ; M.item_let_bind input_of_expr
    ; M.item_let_bind expr_of_output
    ]

let prog (p : 'a Ast.prog) =
  List.map chip p
