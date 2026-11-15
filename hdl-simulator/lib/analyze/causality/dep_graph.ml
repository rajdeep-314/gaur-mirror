open Hdlutils
module Ast = Parse.Ast
module Types = Parse.Types
module Summary = Summary
module Errors = Errors
open Typecheck.Typechecker

module Node = Path

type edge_origin =
  | DirectAssign of Loc.t
  | SubChipCall of
      { chip_name : string
      ; sub_in : Path.t
      ; sub_out : Path.t
      ; loc : Loc.t
      }

module Edge = struct
  type t = edge_origin

  let compare = compare
  let default = DirectAssign Loc.dummy
end

module G = Graph.Persistent.Digraph.ConcreteLabeled (Node) (Edge)
module SCC = Graph.Components.Make (G)

let rec expr_to_path (ei : tc_info Ast.expr_info) : Path.t option =
  match ei.data with
  | Access id -> Some (Path.var id)
  | Member (inner, fld) ->
    Option.map (fun p -> Path.add_field p fld) (expr_to_path inner)
  | _ -> None
;;

let rec vars_of_expr (ei : tc_info Ast.expr_info) : Path.t list =
  match expr_to_path ei with
  | Some path -> [ path ]
  | None ->
    (match ei.data with
     | Const _ -> []
     | Record r_map ->
       StrMap.fold (fun _ fld_ei acc -> vars_of_expr fld_ei @ acc) r_map []
     | Splice (inner, _, _) -> vars_of_expr inner
     | Concat (e1, e2) -> vars_of_expr e1 @ vars_of_expr e2
     | Access id -> [ Path.var id ]
     | Member (inner, _) -> vars_of_expr inner)
;;

let add_edge_with_origin g u origin v =
  let edge = G.E.create u origin v in
  G.add_edge_e g edge
;;

let resolve_subchip_inputs (arg_expr : tc_info Ast.expr_info) (sub_in : Path.t) : Path.t list =
  match Path.match_port "in" sub_in with
  | Some [] ->
    vars_of_expr arg_expr
  | Some (field :: _) ->
    (match arg_expr.data with
     | Record r_map ->
       (match StrMap.find_opt field r_map with
        | Some fld_ei -> vars_of_expr fld_ei
        | None -> [])
     | _ ->
       (match expr_to_path arg_expr with
        | Some p -> [ Path.add_field p field ]
        | None -> vars_of_expr arg_expr))
  | None ->
    vars_of_expr arg_expr
;;

let resolve_subchip_output_target (lhs : Path.t) (sub_out : Path.t) : Path.t =
  match Path.match_port "out" sub_out with
  | Some [] | None -> lhs
  | Some fields -> Path.add_fields lhs fields
;;

let add_assign_edges env g (ai : tc_info Ast.assign_info) =
  let loc = ai.info.loc in
  let (lhs, is_next, rhs) =
    match ai.data with
    | Current (id, r) -> (id, false, r)
    | Next (id, r) -> (id, true, r)
  in
  let target_base = if is_next then Path.next_state lhs else Path.var lhs in
  match rhs.data with
  | Expr expr ->
    (match expr.data with
     | Record r_map when not is_next ->
       StrMap.fold
         (fun fld fld_ei acc_g ->
            let fld_target = Path.add_field target_base fld in
            let deps = vars_of_expr fld_ei in
            let acc_g =
              List.fold_left
                (fun g' dep -> add_edge_with_origin g' dep (DirectAssign loc) fld_target)
                acc_g
                deps
            in
            add_edge_with_origin acc_g fld_target (DirectAssign loc) target_base)
         r_map
         g
     | _ ->
       let deps = vars_of_expr expr in
       List.fold_left
         (fun g' dep -> add_edge_with_origin g' dep (DirectAssign loc) target_base)
         g
         deps)
  | Call (chip_name, arg_expr) ->
    let sub_summary =
      match Summary.Env.find_opt chip_name env with
      | Some s -> s
      | None -> Errors.raise_unbound_chip loc chip_name
    in
    List.fold_left
      (fun acc_g (sub_in, sub_out) ->
         let in_wires = resolve_subchip_inputs arg_expr sub_in in
         let target = resolve_subchip_output_target target_base sub_out in
         let origin = SubChipCall { chip_name; sub_in; sub_out; loc } in
         let acc_g =
           List.fold_left
             (fun g' in_w -> add_edge_with_origin g' in_w origin target)
             acc_g
             in_wires
         in
         if not (Path.equal target target_base) then
           add_edge_with_origin acc_g target (DirectAssign loc) target_base
         else
           acc_g)
      g
      (Summary.paths sub_summary)
;;

let find_cycle_in_scc g scc_nodes =
  let node_set = Hashtbl.create 16 in
  List.iter (fun n -> Hashtbl.add node_set n ()) scc_nodes;
  let in_scc n = Hashtbl.mem node_set n in

  let find_self_edge n =
    let edges = G.find_all_edges g n n in
    match edges with
    | [] -> None
    | edge :: _ -> Some ([ n; n ], [ G.E.label edge ])
  in

  let rec find_path visited curr target =
    if curr = target && visited <> [] then
      Some (List.rev (curr :: visited), [])
    else
      let succ_edges = G.succ_e g curr in
      let filtered =
        List.filter
          (fun e ->
             let dst = G.E.dst e in
             in_scc dst && (dst = target || not (List.mem dst visited)))
          succ_edges
      in
      let rec try_edges = function
        | [] -> None
        | edge :: rest ->
          let next = G.E.dst edge in
          (match find_path (curr :: visited) next target with
           | Some (path, labels) -> Some (path, G.E.label edge :: labels)
           | None -> try_edges rest)
      in
      try_edges filtered
  in

  let rec loop = function
    | [] -> None
    | n :: rest ->
      (match find_self_edge n with
       | Some res -> Some res
       | None ->
         (match find_path [] n n with
          | Some res -> Some res
          | None -> loop rest))
  in
  loop scc_nodes
;;

let format_edge_origin = function
  | DirectAssign loc ->
    if loc = Loc.dummy then "assignment"
    else Fmt.str "assignment at %s" (Loc.range loc)
  | SubChipCall { chip_name; sub_in; sub_out; loc } ->
    if loc = Loc.dummy then
      Fmt.str "call to '%s' (%s -> %s)" chip_name (Path.to_string sub_in) (Path.to_string sub_out)
    else
      Fmt.str "call to '%s' at %s (%s -> %s)" chip_name (Loc.range loc) (Path.to_string sub_in) (Path.to_string sub_out)
;;

let make_pairs xs =
  let rec aux = function
    | [] | [ _ ] -> []
    | x :: (y :: _ as rest) -> (x, y) :: aux rest
  in
  aux xs
;;

let format_cycle_explanations cycle labels =
  let rec aux pairs lbls =
    match pairs, lbls with
    | (u, v) :: p_rest, lbl :: l_rest ->
      let desc = Fmt.str "%s -> %s via %s" (Path.to_string u) (Path.to_string v) (format_edge_origin lbl) in
      desc :: aux p_rest l_rest
    | _ -> []
  in
  aux (make_pairs cycle) labels
;;

let check_cycles_exn chip_loc chip_name g =
  let sccs = SCC.scc_list g in
  List.iter
    (fun scc ->
       match scc with
       | [] -> ()
       | [ single ] ->
         if G.mem_edge g single single then begin
           let edges = G.find_all_edges g single single in
           let lbl = G.E.label (List.hd edges) in
           let cycle = [ single; single ] in
           let expls = format_cycle_explanations cycle [ lbl ] in
           Errors.raise_cycle chip_loc chip_name cycle expls
         end
       | nodes ->
         (match find_cycle_in_scc g nodes with
          | Some (cycle, labels) ->
            let expls = format_cycle_explanations cycle labels in
            Errors.raise_cycle chip_loc chip_name cycle expls
          | None ->
            let cycle = nodes @ [ List.hd nodes ] in
            Errors.raise_cycle chip_loc chip_name cycle []))
    sccs
;;

let is_reachable g src dst =
  if (not (G.mem_vertex g src)) || (not (G.mem_vertex g dst)) then
    false
  else
    let visited = Hashtbl.create 16 in
    let rec dfs curr =
      if curr = dst then true
      else if Hashtbl.mem visited curr then false
      else begin
        Hashtbl.add visited curr ();
        if not (G.mem_vertex g curr) then false
        else List.exists dfs (G.succ g curr)
      end
    in
    dfs src
;;

let port_paths (port : Ast.port) : Path.t list =
  let name = port.name in
  match port.typ with
  | Types.TRecord r_map ->
    StrMap.fold (fun fld _ acc -> Path.port name [ fld ] :: acc) r_map []
  | _ -> [ Path.port name [] ]
;;

let compute_summary (ci : tc_info Ast.chip_info) g : Summary.t =
  let chip = ci.data in
  let in_paths = port_paths chip.input in
  let out_paths = port_paths chip.output in
  let summary = Summary.create chip.name in
  List.fold_left
    (fun acc_sum inp ->
       List.fold_left
         (fun acc outp ->
            if is_reachable g inp outp then
              Summary.add_path ~inp ~outp acc
            else
              acc)
         acc_sum
         out_paths)
    summary
    in_paths
;;

let build_and_check_chip env (ci : tc_info Ast.chip_info) : Summary.t =
  let chip = ci.data in
  let loc = ci.info.loc in
  let in_paths = port_paths chip.input in
  let out_paths = port_paths chip.output in
  let g = G.empty in
  let g = List.fold_left G.add_vertex g in_paths in
  let g = List.fold_left G.add_vertex g out_paths in
  let g =
    StrMap.fold
      (fun st _ acc_g ->
         let acc_g = G.add_vertex acc_g (Path.state st) in
         G.add_vertex acc_g (Path.next_state st))
      chip.states
      g
  in
  let g =
    List.fold_left (add_assign_edges env) g chip.parts
  in
  check_cycles_exn loc chip.name g;
  compute_summary ci g
;;
