module E = Expressions
module F = Functions
module M = Modules
module N = Name
module P = Patterns
module T = Types
module D = Default
module Pr = Pretty
module Te = Toexpr
module Tt = Totype

open Parse
open Hdlutils

module StrMap = Map.Make(String)

let tchip_arg = "chip"
let pchip_next = tchip_arg ^ "'"

let chipvarnext_name id i =
  N.chipfld_name (id ^ "_" ^ (string_of_int i))

let chipvar_name id i =
  String.lowercase_ascii (id ^ "_" ^ (string_of_int i))

let rec typ (t : Types.t) : T.typ =
  match t with
  | TBits   n           -> (T.tid "bv")
  | TRecord tr          -> StrMap.map typ tr |> T.tobject
  | TChip   (tin, tout) -> T.tarrow (typ tin) (typ tout)

let rec expr is_state_var (ei : 'a Ast.expr_info) : E.expr =
  match ei.data with
  | Const  bv           -> expr_const bv
  | Record r            -> StrMap.map (expr is_state_var) r |> E.eobject
  | Splice (e', lb, ub) -> F.splice (expr is_state_var e') (E.eint lb) (E.eint ub)
  | Concat (e1, e2)     -> F.concat (expr is_state_var e1) (expr is_state_var e2)
  | Access id           -> expr_access is_state_var id
  | Member (e', fld)    -> E.eobjmem (expr is_state_var e') fld

  and expr_const bv =
    NonEmpty.to_list bv
    |> List.map E.ebool
    |> E.earray

  and expr_access is_state_var id =
    if is_state_var id
    then E.erecmem (E.eid tchip_arg) id
    else E.eid id

let rhs_out_sl isv id e : E.expr =
  E.eapply
    (E.modpath id N.fun_out)
    [ expr isv e
    ; E.eunit
    ]

let rhs_out_sf i isv id e : E.expr =
  E.eapply
    (E.modpath id N.fun_out)
    [ expr isv e
    ; E.erecmem (E.eid tchip_arg) (chipvar_name id i)
    ]

let rhs_clk_sf i isv id e : E.expr =
  E.eapply
    (E.modpath id N.fun_clk)
    [ expr isv e
    ; E.erecmem (E.eid tchip_arg) (chipvar_name id i)
    ]

let rhs_call i is_stfl isv var id e =
  if is_stfl id
  then
    let eout = E.elet (P.pid var) (rhs_out_sf i isv id e) in
    let eclk = E.elet (P.pid (chipvarnext_name id i)) (rhs_clk_sf i isv id e) in
    fun erest -> eout (eclk erest)
  else
    E.elet (P.pid var) (rhs_out_sl isv id e)

let rhs_expr isv var e =
  E.elet (P.pid var) (expr isv e)

let rhs i is_stfl isv var (ri : 'a Ast.rhs_info) : E.expr -> E.expr =
  match ri.data with
  | Call (id, e) -> rhs_call i is_stfl isv var id e
  | Expr e       -> rhs_expr isv var e

let assign i is_stfl isv (ai : 'a Ast.assign_info) : E.expr -> E.expr =
  match ai.data with
  | Current (id, r)
  | Next    (id, r) -> rhs i is_stfl isv id r


let rec parts' i is_stfl isv (pi : 'a Ast.assign_info list) expr_last : E.expr =
  match pi with
  | []        -> expr_last
  | ai :: ais -> let e  = assign i is_stfl isv ai in
                 let es = parts' (i + 1) is_stfl isv ais expr_last in
                 e es

let parts is_stfl isv pi expr_last =
  parts' 0 is_stfl isv pi expr_last


let tin_typ (ci : 'a Ast.chip_info) : T.typ =
  typ ci.data.input.typ

let tin_decl (ci : 'a Ast.chip_info) : T.typ_decl =
  T.decl_alias N.id_input (tin_typ ci)

let tin_tid = T.tid N.id_input


let tout_typ (ci : 'a Ast.chip_info) : T.typ =
  typ ci.data.output.typ

let tout_decl (ci : 'a Ast.chip_info) : T.typ_decl =
  T.decl_alias N.id_output (tout_typ ci)

let tout_tid = T.tid N.id_output


let tchip_tid = T.tid N.id_state


let expr_in (ci : 'a Ast.chip_info) e =
  let input = ci.data.input.name in
  let bl =
    E.bfun
      N.fun_out_clk
      [ P.pid input, Some tin_tid
      ; P.pid tchip_arg, Some tchip_tid
      ]
      (Some (T.ttuple [tout_tid; tchip_tid]))
  in bl e


let expr_out (ci : 'a Ast.chip_info) =
  E.etuple
    [ E.eid ci.data.output.name
    ; E.eid pchip_next
    ]

let expr_funout (ci : 'a Ast.chip_info) =
  let input = ci.data.input.name in
  E.bfun
    N.fun_out
    [ P.pid input, Some tin_tid
    ; P.pid tchip_arg, Some tchip_tid
    ]
    (Some tout_tid)
  (F.fst (E.eapply (E.eid N.fun_out_clk) [E.eid input; E.eid tchip_arg]))


let expr_funclk (ci : 'a Ast.chip_info) =
  let input = ci.data.input.name in
  E.bfun
    N.fun_clk
    [ P.pid input, Some tin_tid
    ; P.pid tchip_arg, Some tchip_tid
    ]
    (Some tchip_tid)
  (F.snd (E.eapply (E.eid N.fun_out_clk) [E.eid input; E.eid tchip_arg]))


let union (m1 : 'a StrMap.t) (m2 : 'a StrMap.t) =
  StrMap.fold (fun k v acc -> StrMap.add k v acc) m1 m2


let rec sf_parts'' i is_stfl (pi : 'a Ast.assign_info list) =
  match pi with
  | [] -> []
  | ai :: ais ->
    match ai.data with
    | Current (_, ri)
    | Next    (_, ri) ->
      match ri.data with
      | Call (id, e) ->
        let tid = T.tmodpath id N.id_state in
        if is_stfl id
        then (id, i, tid) :: sf_parts'' (i + 1) is_stfl ais
        else sf_parts'' (i + 1) is_stfl ais
      | Expr e -> sf_parts'' (i + 1) is_stfl ais

let sf_parts' is_stfl pi = sf_parts'' 0 is_stfl pi

let tchip_decl isv is_stfl (ci : 'a Ast.chip_info) =
  let r = sf_parts' is_stfl ci.data.parts
          |> List.map (fun (id, i, tid) -> (chipvar_name id i, tid))
          |> StrMap.of_list
          |> union (StrMap.map typ ci.data.states)
  in
  if StrMap.is_empty r
  then T.decl_alias  N.id_state T.tunit
  else T.decl_record N.id_state r

let rec sf_default' i is_stfl (pi : 'a Ast.assign_info list) =
  match pi with
  | [] -> []
  | ai :: ais ->
    match ai.data with
    | Current (_, ri)
    | Next    (_, ri) ->
      match ri.data with
      | Call (id, e) ->
        let tid = E.modpath id N.fun_default_state in
        if is_stfl id
        then (chipvar_name id i, tid) :: sf_default' (i + 1) is_stfl ais
        else sf_default' (i + 1) is_stfl ais
      | Expr e -> sf_default' (i + 1) is_stfl ais

let sf_default is_stfl pi =
  sf_default' 0 is_stfl pi
  |> StrMap.of_list

let default is_stfl (ci : 'a Ast.chip_info) =
  let r = ci.data.states
          |> StrMap.map (fun typ -> D.default_expr typ)
          |> union (sf_default is_stfl ci.data.parts)
  in
  if StrMap.is_empty r
  then E.eunit
  else E.erecord r

let expr_def_t is_stfl ci =
  E.bfun N.fun_default_state [] (Some tchip_tid) (default is_stfl ci)

let expr_def_tin (ci : 'a Ast.chip_info) =
  E.bfun N.fun_default_input [] (Some (T.tid N.id_input)) (D.default_tin ci)

let expr_record isv is_stfl (ci : 'a Ast.chip_info) expr_last =
  let erec_states =
    ci.data.states
    |> StrMap.to_list
    |> List.map (fun (f, _) -> f, f ^ "'" |> E.eid)
    |> StrMap.of_list
  in

  let erec_parts =
    sf_parts' is_stfl ci.data.parts
    |> List.map (fun (id, i, _) -> (chipvar_name id i, chipvarnext_name id i |> E.eid))
    |> StrMap.of_list
  in

  let erec = E.erecord (union erec_states erec_parts) in
  E.elet (P.pid pchip_next) erec expr_last

let isv_ci (ci : 'a Ast.chip_info) var =
  StrMap.mem var ci.data.states

let chip is_stfl (ci : 'a Ast.chip_info) =
  let isv = isv_ci ci in
  let ein   = expr_in ci in
  let eout  = expr_out ci in
  let eparts = parts is_stfl isv ci.data.parts in
  let eupdate = expr_record isv is_stfl ci in
  let efout = expr_funout ci in
  let efclk = expr_funclk ci in
  let edef_t = expr_def_t is_stfl ci in
  let edef_tin = expr_def_tin ci in
  let epp_tin = Pr.pretty_tin ci in
  let epp_tout = Pr.pretty_tout ci in
  let eip_typ = Te.input_type ci in
  let ett_tin = Tt.totype_tin ci in
  let ett_tout = Tt.totype_tout ci in

  M.mdecl
    ci.data.name
    [ M.item_typ_decl (tchip_decl isv is_stfl ci)
    ; M.item_let_bind edef_t
    ; M.item_typ_decl (tin_decl ci)
    ; M.item_let_bind edef_tin
    ; M.item_typ_decl (tout_decl ci)
    ; M.item_let_bind (ein (eparts (eupdate eout)))
    ; M.item_let_bind efout
    ; M.item_let_bind efclk
    ; M.item_let_bind epp_tin
    ; M.item_let_bind epp_tout
    ; M.item_let_bind eip_typ
    ; M.item_let_bind ett_tin
    ; M.item_let_bind ett_tout
    ]

let prog is_stfl (p : 'a Ast.prog) =
  List.map (chip is_stfl) p
