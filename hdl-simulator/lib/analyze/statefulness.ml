open Typecheck
open Parse
open Adt
open Hdlutils

module Dep = Dependency

module StrMap = Map.Make(String)

(* Assuming the program is in topological order *)

module type STF = sig
  val is_stateful : string -> bool
end


module type PROG = sig
  type t
  val prog : t Ast.prog
end


module Make (P : PROG) () : STF = struct
  exception Not_foundd of string

  module Chipinfo = struct type t = P.t Ast.chip_info end
  module Chiptbl  = Tbl.Make (String) (Chipinfo)
  module Stftbl   = Tbl.Make (String) (Bool)

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

  let rec is_stateful_node n =
    let ci = Chiptbl.find n in
    if is_nand ci.data.name then false
    else if has_states ci then true
    else
      Dep.Chips.chip ci
      |> List.map (fun id ->
        match StrMap.find_opt id chip_node_map with
        | None   -> raise (Not_foundd id)
        | Some x -> x)
      |> List.exists is_stateful_node

  let is_stateful id =
    match StrMap.find_opt id chip_node_map with
    | None   -> raise (Not_foundd id)
    | Some x -> is_stateful_node x
end
