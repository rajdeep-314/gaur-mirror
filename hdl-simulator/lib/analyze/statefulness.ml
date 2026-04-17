open Typecheck
open Parse
open Adt
module Dep = Dependency

module StrMap = Map.Make(String)

(* Assuming the program is in topological order *)

module Make (P : sig type a val prog : a Ast.prog end) () : sig
  val is_stateful : P.a Ast.chip_info -> bool
end = struct
  let chiptbl : P.a Chiptbl.t = Chiptbl.make P.prog

  (* The memoization table: key is chip name, value is statefulness *)
  let v : (string, bool) Hashtbl.t = Hashtbl.create (List.length P.prog)

  let is_nand (ci : P.a Ast.chip_info) : bool =
    ci.data.name = Init.nand_name

  let has_states (ci : P.a Ast.chip_info) : bool =
    not (StrMap.is_empty ci.data.states)

  let rec is_stateful (ci : P.a Ast.chip_info) : bool =
    let name = ci.data.name in
    match Hashtbl.find_opt v name with
    | Some sf -> sf
    | None    ->
        let sf = compute_stateful ci in
        Hashtbl.add v name sf;
        sf

  and compute_stateful (ci : P.a Ast.chip_info) : bool =
    if is_nand ci then false
    else if has_states ci then true
    else
      let deps = Dep.Chips.chip ci in
      List.exists
        (fun name -> is_stateful (Chiptbl.find chiptbl name))
        deps
end
