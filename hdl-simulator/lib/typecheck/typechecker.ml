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
  let tci = mk_tc_info loc typ env in
  Ast.mk_expr_info te tci
