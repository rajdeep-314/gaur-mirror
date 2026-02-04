open Dep_tree
open Type_check

module Dependencies = struct
  let of_assign t_assign =
    let open Typedtree in
    let expr = t_assign.assign_desc.expr.expr_desc in
    match expr with
    | Texpr_constant _
    | Texpr_splice (_, _, _)
    | Texpr_concat (_, _)
    | Texpr_record _
    | Texpr_access _
    | Texpr_member (_, _) -> []
    | Texpr_apply (chip, _) -> [chip]

  let of_chip t_chip =
    let open Typedtree in
    List.fold_left
      (fun cs a -> cs @ of_assign a)
      []
      t_chip.parts
end


module MkGraph = struct
  let of_chip g t_chip =
    let deps = Dependencies.of_chip t_chip in
    let name = t_chip.name in
    List.fold_left
      (fun g v -> DepGraph.add_edge g v name)
      g
      deps

  let of_program (t_program: Typedtree.program) =
    List.fold_left
      of_chip
      DepGraph.empty
      t_program
end

let find_cycles g =
  DepGraphJohnson.fold_cycles
    (fun c cs -> c :: cs)
    g
    []

let dep_check_program t_program =
  let open Errors in
  let g = MkGraph.of_program t_program in
  let cycles = find_cycles g in
  match cycles with
  | [] -> ()
  | _ ->
    Printf.eprintf "%s" (Printers.err (Cycles cycles));
    exit 1
