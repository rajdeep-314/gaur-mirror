open Typecheck
open Parse
open Hdlutils

module Dep = Dependency

module StrMap = Map.Make(String)

(* Assuming the program is in topological order *)


module Errors = struct
  type t =
    | Unbound_Chip of string

  exception SfError of t

  let raise_unbound_chip id =
    raise (SfError (Unbound_Chip id))

  let pp ppf err =
    match err with
    | Unbound_Chip id ->
      Fmt.pf ppf
        "Could not determine statefulness of unbound chip %s"
end


module type STF = sig
  val is_stateful : string -> bool
end


module type PROG = sig
  type t
  val prog : t Ast.prog
end


module Make (P : PROG) () : STF = struct
  module Chipinfo = struct type t = P.t Ast.chip_info end
  module Chiptbl  = Tbl.Make (String) (Chipinfo) ()
  module Stftbl   = Tbl.Make (String) (Bool) ()

  open Errors

  let add_prog () =
    List.map
      (fun (ci : P.t Ast.chip_info) -> (ci.data.name, ci))
      P.prog
    |> Chiptbl.add_list

  let chip_node_list = add_prog ()
  let chip_node_map  = StrMap.of_list chip_node_list

  let is_nand id =
    id = Init.nand_name

  let has_states (ci : 'a Ast.chip_info) =
    not (StrMap.is_empty ci.data.states)

  let find_exn id =
    match StrMap.find_opt id chip_node_map with
    | None   -> raise_unbound_chip id
    | Some n -> n

  let rec is_stateful_node n =
    let ci = Chiptbl.find n in
    if is_nand ci.data.name then false
    else if has_states ci then true
    else
      Dep.Chips.chip ci
      |> List.map (fun id -> find_exn id)
      |> List.exists is_stateful_node

  let is_stateful id =
    find_exn id
    |> is_stateful_node
end
