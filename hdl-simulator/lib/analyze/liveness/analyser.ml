open Parse
open Var
open Errors

module VarSet = Set.Make(Var)
module StrMap = Map.Make(String)
module StrSet = Set.Make(String)

type genkill =
  { gen  : VarSet.t
  ; kill : VarSet.t
  }

module Use = struct
  let rec expr stvars (e : 'a Ast.expr_info) : VarSet.t =
    match e.data with
    | Const  bv           -> VarSet.empty
    | Record r            -> expr_record stvars r
    | Splice (e', lb, ub) -> expr stvars e'
    | Concat (e1, e2)     -> VarSet.union (expr stvars e1) (expr stvars e2)
    | Access id           -> expr_access stvars id (* VarSet.singleton (current id e.info) *)
    | Member (e', fld)    -> expr stvars e'

  and expr_record stvars r =
    let vars =
      r
      |> StrMap.to_list
      |> List.map (fun (_, ei) -> ei)
      |> List.map (expr stvars)
    in
    List.fold_left VarSet.union VarSet.empty vars

  and expr_access stvars id =
    if StrSet.mem id stvars
    then VarSet.singleton (current id)
    else VarSet.singleton (nostate id)

  let rhs stvars (r : 'a Ast.rhs_info) : VarSet.t =
    match r.data with
    | Call (_, ei) -> expr stvars ei
    | Expr ei      -> expr stvars ei

  let assign stvars (a : 'a Ast.assign_info) : VarSet.t =
    match a.data with
    | Current (_, ri) -> rhs stvars ri
    | Next    (_, ri) -> rhs stvars ri

  let first (ci : 'a Ast.chip_info) : VarSet.t = VarSet.empty
  let last (ci : 'a Ast.chip_info) : VarSet.t =
    let outname = (Ast.Mnemonics.chip_info_output ci).name in
    let outvar = nostate outname in
    let states =
      ci.data.states
      |> StrMap.to_list
      |> List.map fst
      |> List.map next
      |> VarSet.of_list
    in
    VarSet.add outvar states
end


module Def = struct
  let assign stvars (a : 'a Ast.assign_info) : VarSet.t =
    match a.data with
    | Current (id, _) ->
      if StrSet.mem id stvars
      then VarSet.singleton (current id)
      else VarSet.singleton (nostate id)

    | Next (id, _) ->
      if StrSet.mem id stvars
      then VarSet.singleton (next id)
      else VarSet.singleton (nostate id)

  let first (ci : 'a Ast.chip_info) : VarSet.t =
    let inname = (Ast.Mnemonics.chip_info_input ci).name in
    let invar = nostate inname in
    let states =
      ci.data.states
      |> StrMap.to_list
      |> List.map fst
      |> List.map current
      |> VarSet.of_list
    in
    VarSet.add invar states

  let last (ci : 'a Ast.chip_info) : VarSet.t = VarSet.empty
end

(* use ∩ kill is nonempty then a variable is used before being defined *)
let use_before_def ~use ~kill ~loc =
  let set = VarSet.inter use kill in
  if not (VarSet.is_empty set)
  then raise_use_before_def set loc

(* def ∩ kill is nonempty then a variable is defined more than once *)
let multiple_def ~def ~kill ~loc =
  let set = VarSet.inter def kill in
  if not (VarSet.is_empty set)
  then raise_multiple_def set loc

(* use ∩ def is nonempty then a variable is used and defined at the same time *)
let use_with_def ~use ~def ~loc =
  let set = VarSet.inter use def in
  if not (VarSet.is_empty set)
  then raise_use_with_def set loc

let unused ~def ~kill ~gen ~loc =
  let set = VarSet.diff (VarSet.diff def kill) gen in
  if not (VarSet.is_empty set)
  then raise_unused def loc

let undefined ~gen ~loc =
  if not (VarSet.is_empty gen)
  then raise_undefined gen loc

let rec gen_kill ~loc stvars last (parts : 'a Ast.assign_info list) : genkill =
  match parts with
  | [] -> last

  | ai :: ais ->
    let { gen; kill } = gen_kill ~loc:loc stvars last ais in
    let use = Use.assign stvars ai in
    let def = Def.assign stvars ai in
    (* let () = Fmt.(list Var.Pretty.pp_var stderr) (VarSet.to_list gen) in
    let () = Fmt.(list Var.Pretty.pp_var stderr) (VarSet.to_list kill) in *)
    use_before_def ~use:use ~kill:kill ~loc:loc;
    multiple_def ~def:def ~kill:kill ~loc:loc;
    use_with_def ~use:use ~def:def ~loc:loc;
    unused ~def:def ~kill:kill ~gen:gen ~loc:loc;
    let gen' = VarSet.union (VarSet.diff gen def) use in
    let kill' = VarSet.union kill def in
    { gen=gen'
    ; kill=kill'
    }

let analyse_chip (ci : Typecheck.Typechecker.tc_info Ast.chip_info) =
  let stvars =
    ci.data.states
    |> StrMap.to_list
    |> List.map fst
    |> StrSet.of_list
  in

  (* let _ =
    ci.data.states
    |> StrMap.to_list
    |> List.map fst
    |> Fmt.(list string stderr)
  in *)

  let last =
    { gen=Use.last ci
    ; kill=Def.last ci
    }
  in

  let loc = ci.info.loc in

  let parts = ci.data.parts in
  let use = Use.first ci in
  let def = Def.first ci in
  let { gen; kill } = gen_kill ~loc:loc stvars last parts in
  let gen' = VarSet.union (VarSet.diff gen def) use in
  (* let kill' = VarSet.union kill def in *)
  undefined ~gen:gen' ~loc:loc

let analyse_prog (p : 'a Ast.prog) =
  List.iter analyse_chip p
