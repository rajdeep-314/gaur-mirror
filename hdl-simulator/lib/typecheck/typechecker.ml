open Hdlutils
open Parse
open Errors

module StrMap = Stdlib.Map.Make(String)

type tc_info =
  { loc : Loc.t
  ; typ : Types.t
  ; env : Env.t
  }

let mk_tc_info loc typ env =
  { loc=loc
  ; typ=typ
  ; env=env
  }

let rec tc_expr env (ei : Loc.t Ast.expr_info) : tc_info Ast.expr_info =
  let loc = ei.info in
  let pe  = ei.data in
  let (te, typ) : tc_info Ast.expr * Types.t =

  match pe with
  | Const bv ->
    let te  = Ast.Const bv in
    let typ = Types.TBits (NonEmpty.length bv) in
    (te, typ)

  | Record pr ->
    let tr  = StrMap.map (tc_expr env) pr in
    let te  = Ast.Record tr in
    let typ = Types.TRecord (StrMap.map (fun x -> x.Info.info.typ) tr) in
    (te, typ)

  | Splice (pe', lb, ub) ->
    let te' = tc_expr env pe' in
    begin match te'.info.typ with
    | TBits l ->
      if (0 <= lb) && (lb <= ub) && (ub <= l)
      then (Splice (te', lb, ub), TBits(ub - lb + 1))
      else raise_splice_bounds loc lb ub l
    | TRecord _
    | TChip _ -> raise_splice_type loc te'.info.typ
    end

  | Concat (pe1, pe2) ->
    let te1 = tc_expr env pe1 in
    let te2 = tc_expr env pe2 in
    begin match te1.info.typ, te2.info.typ with
    | TBits l1, TBits l2 -> (Concat (te1, te2), TBits (l1 + l2))
    | _ -> raise_concat_type loc te1.info.typ te2.info.typ
    end

  | Access id ->
    begin match Env.find_opt id env with
    | Some t -> (Access id, t)
    | None -> raise_access loc id
    end

  | Member (pe', fld) ->
    let te' = tc_expr env pe' in
    begin match te'.info.typ with
    | TRecord r ->
      begin match StrMap.find_opt fld r with
      | Some t -> (Member (te', fld), t)
      | None -> raise_member_field loc fld
      end
    | TBits _
    | TChip _ -> raise_member_type loc te'.info.typ
    end
  in
  let info = mk_tc_info loc typ env in
  Ast.mk_expr_info te info


let tc_rhs env (ri : Loc.t Ast.rhs_info) : tc_info Ast.rhs_info =
  let loc = ri.info in
  let pr  = ri.data in
  let (tr, typ) : tc_info Ast.rhs * Types.t =
  match pr with
  | Call (id, pe) ->
    begin match Env.find_opt id env with
    | Some tid ->
      begin match tid with
      | TChip (tin, tout) ->
        let te   = tc_expr env pe in
        let targ = te.info.typ in
        if Types.eq tin targ
        then (Call (id, te), tout)
        else raise_call_arg loc targ tin

      | TBits _
      | TRecord _ -> raise_call_typ loc tid
      end

    | None -> raise_call_unbound loc id
    end

  | Expr pe ->
    let te = tc_expr env pe in
    (Expr te, te.info.typ)
  in
  let info = mk_tc_info loc typ env in
  Ast.mk_rhs_info tr info


let tc_assign env (ai : Loc.t Ast.assign_info) : tc_info Ast.assign_info =
  let loc = ai.info in
  let (id, rhs, build) =
  match ai.data with
  | Current (id, rhs) -> (id, rhs, fun r' -> Ast.Current (id, r'))
  | Next    (id, rhs) -> (id, rhs, fun r' -> Ast.Next    (id, r'))
  in
  let tr = tc_rhs env rhs in
  let tr_typ = tr.info.typ in
  let env' =
  match Env.find_opt id env with
  | Some tid ->
    if Types.eq tr_typ tid
    then env
    else raise_assign loc tr_typ tid

  | None -> Env.add id tr_typ env
  in
  let info = mk_tc_info loc tr_typ env' in
  Ast.mk_assign_info (build tr) info


let add_in_port (ci : Loc.t Ast.chip_info) env =
  let id  = ci.data.input.name in
  let typ = ci.data.input.typ  in
  Env.add id typ env

let add_out_port (ci : Loc.t Ast.chip_info) env =
  let id  = ci.data.output.name in
  let typ = ci.data.output.typ  in
  Env.add id typ env

let add_states (ci : Loc.t Ast.chip_info) env =
  match ci.data.states with
  | None   -> env
  | Some s -> StrMap.fold Env.add s env

let add_port_states ci env = env
                             |> add_in_port ci
                             |> add_out_port ci
                             |> add_states ci

let rec tc_parts env ais =
  match ais with
  | []         -> [], env
  | pa :: pas  -> let ta   = tc_assign env pa in
                  let env' = ta.info.env      in
                  let (tas, env') = tc_parts env' pas in
                  ta :: tas, env'

let tc_chip env (ci : Loc.t Ast.chip_info) =
  let loc = ci.info in
  let pc  = ci.data in
  let pprt = ci.data.parts in
  let env' = add_port_states ci env in
  let tprt, env'' = tc_parts env' pprt in
  let tc = { pc with parts = tprt } in
  let tci = mk_tc_info loc pc.output.typ env'' in
  Ast.mk_chip_info tc tci
