open Errors
open Dependency
open Parse
open Hdlutils

module STRING = struct
  type t = string

  let compare = String.compare
  let hash = Hashtbl.hash
  let equal = String.equal
end

module G = Graph.Persistent.Digraph.Concrete(STRING)
module T = Graph.Topological.Make(G)
module C = Graph.Cycles.Johnson(G)

module StrMap = Map.Make(String)

let add_vertex g (c, _) =
  G.add_vertex g c

let add_dep c g dep =
  G.add_edge g c dep

let add_deps g (c, dep_list) =
  List.fold_left (add_dep c) g dep_list

let cycles g =
  C.fold_cycles (fun cycle acc -> cycle :: acc) g []

let sorted g =
  T.fold (fun chip acc -> chip :: acc) g []

let name_loc_map (p : Loc.t Ast.prog) =
  p
  |> List.map (fun ci -> Ast.Mnemonics.chip_info_name ci, ci.info)
  |> StrMap.of_list

let rec embed_loc_cycle map c =
  match c with
  | []            -> []
  | chip :: chips ->
    match StrMap.find_opt chip map with
    | None -> raise_unbound_chip chip
    | Some item -> (chip, item) :: embed_loc_cycle map chips

let embed_loc_cycles map cs =
  cs
  |> List.map (embed_loc_cycle map)

let name_info_map (p : 'a Ast.prog) =
  p
  |> List.map (fun ci -> Ast.Mnemonics.chip_info_name ci, ci)
  |> StrMap.of_list

let rec to_info_list map p =
  match p with
  | []          -> []
  | chip::chips ->
    if chip = Typecheck.Init.nand_name
    then to_info_list map chips
    else
      (match StrMap.find_opt chip map with
      | None -> raise_unbound_chip chip
      | Some s -> s :: to_info_list map chips)

let toposort (p : Loc.t Ast.prog) : Loc.t Ast.prog =
  let deps = Chips.chip_all p in
  let map_loc = name_loc_map p in
  let map_info = name_info_map p in
  let g = G.empty in
  let g = List.fold_left (fun acc (name, _) -> G.add_vertex acc name) g deps in
  let g = List.fold_left add_deps g deps in
  match cycles g with
  | [] ->
    sorted g
    |> to_info_list map_info
  | cs ->
    cs
    |> embed_loc_cycles map_loc
    |> report_cycles
