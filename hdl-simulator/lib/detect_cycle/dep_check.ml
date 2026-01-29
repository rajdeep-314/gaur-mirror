open Dep_tree
open Type_check

let report_cycle c =
  Printf.eprintf
    (* "Error: Cyclic chip dependency between:\n%s\n" *)
    "%s\n"
    (List.fold_left
       (fun s c -> Printf.sprintf "%s -- %s"s c)
       (List.hd c)
       (List.tl c)
    )

let report_cycles cs =
  Printf.eprintf "Error: Cyclic chip dependency between:\n";
  List.iter
    report_cycle
    cs

let chip_from_assign cl (t_assign: Typedtree.assign)  =
  let expr = t_assign.assign_desc.expr.expr_desc in
  match expr with
  | Texpr_constant _
  | Texpr_splice (_, _, _)
  | Texpr_concat (_, _)
  | Texpr_record _
  | Texpr_access _
  | Texpr_member (_, _) -> cl
  | Texpr_apply (chip, _) -> chip :: cl


let dependents_chip (t_chip: Typedtree.chip) =
  List.fold_left
    chip_from_assign
    []
    t_chip.parts


let md_graph_chip g (t_chip: Typedtree.chip) =
  let chip = t_chip.name in
  let deps = dependents_chip t_chip in
  List.fold_left
    (fun g v -> DepGraph.add_edge g v chip)
    g
    deps


let mk_graph_program (t_program: Typedtree.program) =
  List.fold_left
    md_graph_chip
    DepGraph.empty
    t_program


let find_cycles g =
  DepGraphJohnson.fold_cycles
    (fun c cs -> c :: cs)
    g
    []

let dep_check_program (t_program: Typedtree.program) =
  let g = mk_graph_program t_program in
  let cycles = find_cycles g in
  match cycles with
  | [] -> ()
  | _ ->
    report_cycles cycles;
    exit 1
