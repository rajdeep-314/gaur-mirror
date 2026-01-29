open Parse.Asttypes
open Usedef_tree
open Typing

module StrSet = Set.Make(String)

exception UnreachablePath of string

type error =
  (* Use-Def Errors *)
  | RepeatingDefinition of ident
  | OutVariableUsage of ident
  | UnboundVariable of ident
  | InVariableAssignment of ident
  | StateVariableCurrentAssignment of ident
  | InternalVariableMultipleAssignment of ident
  | StateVariableMultipleNextAssignment of ident
  | OutVariableMultipleAssignment of ident
  | NonStateNextAssignment of ident
  | UnassignedOutVariable of ident
  | UnupdatedStateVariable of ident

let report_error = function
  | RepeatingDefinition ident ->
    Printf.eprintf
      "Error: Repeating definition of variable %s.\n"
      ident

  | OutVariableUsage ident ->
    Printf.eprintf
      "Error: Output variable (%s) can not be used inside an expression.\n"
      ident

  | UnboundVariable ident ->
    Printf.eprintf
      "Error: Unbound variable %s.\n"
      ident

  | InVariableAssignment ident ->
    Printf.eprintf
      "Error: Can not assign any expression to an input variable (%s).\n"
      ident

  | StateVariableCurrentAssignment ident ->
    Printf.eprintf
      "Error: Can not assign a state variable (%s) combinationally.\n"
      ident;
    Printf.eprintf
      "Use `<-` instead."

  | InternalVariableMultipleAssignment ident ->
    Printf.printf
      "Error: Internal variable (%s) can not be assigned more than once (SSA).\n"
      ident

  | StateVariableMultipleNextAssignment ident ->
    Printf.eprintf
      "Error: State variable (%s) can not be updated more than once.\n"
      ident

  | OutVariableMultipleAssignment ident ->
    Printf.eprintf
      "Error: Output variable (%s) can not be assigned more than once.\n"
      ident

  | NonStateNextAssignment ident ->
    Printf.eprintf
      "Error: The variable (%s) is not a state variable,\
      \ thus can not be assigned using `<-`."
      ident;
    Printf.eprintf
      "Use `:=` instead, or make it a STATE variable.\n"

  | UnassignedOutVariable ident ->
    Printf.eprintf
      "Error: The output variable (%s) is not left unassigned.\n"
      ident

  | UnupdatedStateVariable ident ->
    Printf.eprintf
      "Error: The state variable (%s) is not updated.\n"
      ident

exception Error of error

type config_error =
  (* Configuration Errors *)
  | InVariableInDegree of ident
  | OutVariableOutDegree of ident
  | CurrentStateVariableInDegree of ident
  | OutVariableInDegree of ident
  | NextStateVariableInDegree of ident
  | NextStateVariableOutDegree of ident
  | InternalVariableInDegree of ident


let report_config_error = function
  | InVariableInDegree ident ->
    Printf.eprintf
      "[BUG] In-degree of an input variable (%s) must be 0.\n"
      ident

  | OutVariableOutDegree ident ->
    Printf.eprintf
      "[BUG] Out-degree of an output variable (%s) must be 0.\n"
      ident

  | CurrentStateVariableInDegree ident ->
    Printf.eprintf
      "[BUG] In-degree of a current-state variable (%s) must be 0.\n"
      ident

  | OutVariableInDegree ident ->
    Printf.eprintf
      "[BUG] In-degree of an output variable (%s) must be atmost 1.\n"
      ident

  | NextStateVariableInDegree ident ->
    Printf.eprintf
      "[BUG] In-degree of a next-state variable (%s) must be atmost 1.\n"
      ident

  | NextStateVariableOutDegree ident ->
    Printf.eprintf
      "[BUG] Out-degree of a next-state variable (%s) must be 0.\n"
      ident

  | InternalVariableInDegree ident ->
    Printf.eprintf
      "[BUG] In-degree of an internal variable (%s) must be 1.\n"
      ident

exception ConfigError of config_error

type warning =
  | UnusedInVariable of ident
  | UnusedStateVariable of ident
  | UnusedInternalVariable of ident

let report_warning = function
  | UnusedInVariable ident ->
    Printf.eprintf
      "Warning: Unused input variable %s\n"
      ident
  | UnusedStateVariable ident ->
    Printf.eprintf
      "Warning: Unused state variable %s\n"
      ident
  | UnusedInternalVariable ident ->
    Printf.eprintf
      "Warning: Unused internal variable %s\n"
      ident

let rec vars_from_expr (expr: Typedtree.expr) =
  match expr.expr_desc with
  | Texpr_constant _ -> StrSet.empty
  | Texpr_apply (_, expr) -> vars_from_expr expr
  | Texpr_splice (expr, _, _) -> vars_from_expr expr
  | Texpr_concat (expr1, exrp2) -> StrSet.union (vars_from_expr expr1) (vars_from_expr exrp2)
  | Texpr_record record -> StrMap.fold (fun _ expr acc -> StrSet.union (vars_from_expr expr) acc) record StrSet.empty
  | Texpr_access var -> StrSet.singleton var
  | Texpr_member (expr, _) -> vars_from_expr expr


let initial_graph (t_chip: Typedtree.chip) =
  let var_in = t_chip.input.port_name in
  let var_out = t_chip.output.port_name in

  let chip_states = Option.value t_chip.states ~default:StrMap.empty in

  let var_states = StrMap.map (fun _ -> ()) chip_states in
  let var_states_list =
    List.map
      (fun (ident, _) -> ident)
      (StrMap.to_list var_states)
  in

  let () =
    begin
      if var_in = var_out then raise (Error (RepeatingDefinition var_in))
      else
      if StrMap.mem var_in var_states then raise (Error (RepeatingDefinition var_in))
      else
      if StrMap.mem var_out var_states then raise (Error (RepeatingDefinition var_out))
      else ()
    end
  in

  let var_in_catg = UDGraphV.{name=var_in; catg=Input} in
  let var_out_catg = UDGraphV.{name=var_out; catg=Output} in
  let var_states_catg =
    List.map
      (fun s -> UDGraphV.{name=s; catg=CurrentState})
      var_states_list
  in
  let var_states_primed_catg =
    List.map
      (fun s -> UDGraphV.{name=s; catg=NextState})
      var_states_list
  in

  let g = UDGraph.empty in
  let g = UDGraph.add_vertex g var_in_catg in
  let g = UDGraph.add_vertex g var_out_catg in
  let g = List.fold_left UDGraph.add_vertex g var_states_catg in
  let g = List.fold_left UDGraph.add_vertex g var_states_primed_catg in
  g


let initial_graph_info (t_chip: Typedtree.chip) =
  let g = initial_graph t_chip in
  let chip_states = Option.value t_chip.states ~default:StrMap.empty in
  let states_info = StrMap.map (fun _ -> mk_state_info) chip_states in
  let bound_internals = StrSet.empty in

  { graph=g;
    chip=t_chip.name;
    var_in=t_chip.input.port_name;
    var_out=t_chip.output.port_name;
    out_info=mk_out_info;
    states_info=states_info;
    bound_internals=bound_internals
  }


let upd_graph_assignment udgi (t_assign: Typedtree.assign) =
  let expr = t_assign.assign_desc.expr in
  let vars = vars_from_expr expr in

  (* Check if a variable appearing on the RHS of an assignment are bound.
     Return its category if it is, otherwise raise error appropriately.
     The following variables can appear on the RHS:
     - In variable
     - State variables
     - Bound internal variables

     Out variable and unbound internal variables can not appear on the RHS
  *)
  let check_boundedness udgi var =
    if udgi.var_in = var then Input
    else
    if udgi.var_out = var then raise (Error (OutVariableUsage var))
    else
    if StrMap.mem var udgi.states_info then CurrentState
    else
    if StrSet.mem var udgi.bound_internals then Internal
    else
      raise (Error(UnboundVariable var))
  in

  (* Get a list of all variables on the RHS of an assignment, with their
     respective categories *)
  let vars_vertices =
    List.map
      (fun var -> UDGraphV.{name=var; catg=check_boundedness udgi var})
      (StrSet.to_list vars)
  in

  (* The variable being bound by the assignment *)
  let assignee = t_assign.assign_desc.var in

  (* Validate assignment, in case it is a current state assignment, and
     return the category of the assignee appropriately.
     - In variables can not be assigned
     - Bound out variables can not be assigned
     - State variables can not be assigned sequentially
     - Bound internal variables can not be assigned
  *)
  let check_assignee_current udgi var =
    if udgi.var_in = var then raise (Error(InVariableAssignment var))
    else
    if udgi.var_out = var then
      match udgi.out_info.bound with
      | Unbound -> Output
      | Bound -> raise (Error (OutVariableMultipleAssignment var))
    else
    if StrMap.mem var udgi.states_info
    then raise (Error(StateVariableCurrentAssignment var))
    else
    if StrSet.mem var udgi.bound_internals then
      raise (Error(InternalVariableMultipleAssignment var))
    else
      Internal
  in

  (* Validate assignment, in case it is a next state assignment.
     - Updated state variables can not be assigned
     - No other variable categories can be assigned sequentially
  *)
  let check_assignee_next udgi var =
    if StrMap.mem var udgi.states_info then
      match (StrMap.find var udgi.states_info).updated with
      | Updated -> raise (Error (StateVariableMultipleNextAssignment var))
      | Unupdated -> NextState
    else
      raise (Error (NonStateNextAssignment var))
  in

  let assignee_catg =
    match t_assign.assign_desc.state with
    | Current -> check_assignee_current udgi assignee
    | Next -> check_assignee_next udgi assignee
  in

  let assignee_vertex = UDGraphV.{name=assignee; catg=assignee_catg} in

  (* Modify the graph by adding a vertex for assignee, and edges from all the
     vertices from the RHS of the assignment to the vertex of the assignee
  *)
  let g = udgi.graph in
  let g' = UDGraph.add_vertex g assignee_vertex in
  let g' =
    List.fold_left
      (fun g v -> UDGraph.add_edge g v assignee_vertex)
      g'
      vars_vertices
  in

  (* Update the graph metadata, if the assignee category is
     - Output, then update its info to being bound
     - Internal, then add it to the bound_internals set
     - NextState, then update its info to being bound
  *)
  let udgi' =
    match assignee_catg with
    | Output ->
      { udgi with
        out_info={bound=Bound}
      }
    | Internal ->
      { udgi with
        bound_internals=StrSet.add assignee udgi.bound_internals
      }
    | NextState ->
      { udgi with
        states_info=StrMap.add assignee {updated=Updated} udgi.states_info
      }
    | _ -> raise (UnreachablePath "unreachable path")
  in

  let udgi' = {udgi' with graph=g'} in
  udgi'


let mk_graph_chip (t_chip: Typedtree.chip) =
  let g = initial_graph_info t_chip in
  let g' = List.fold_left
      upd_graph_assignment
      g
      t_chip.parts
  in
  g'


let mk_graph_program (t_program: Typedtree.program) =
  List.map mk_graph_chip t_program


let config_check (g: UDGraph.t) =
  let config_check_vertex (v: UDGraphV.t) =
    let in_d = UDGraph.in_degree g v in
    let out_d = UDGraph.out_degree g v in
    match v.catg with
    | Input ->
      if in_d > 0 then raise (ConfigError (InVariableInDegree v.name))
      else ()
    | Output ->
      if in_d >= 2 then raise (ConfigError (OutVariableInDegree v.name))
      else
      if out_d > 0 then raise (ConfigError (OutVariableOutDegree v.name))
      else ()
    | CurrentState ->
      if in_d > 0 then raise (ConfigError (CurrentStateVariableInDegree v.name))
      else ()
    | NextState ->
      if in_d >= 2 then raise (ConfigError (NextStateVariableInDegree v.name))
      else
      if out_d > 0 then raise (ConfigError (NextStateVariableOutDegree v.name))
      else ()
    | Internal ->
      if in_d != 1 then raise (ConfigError (InternalVariableInDegree v.name))
      else ()
  in
  UDGraph.iter_vertex config_check_vertex g


let ud_check_chip (t_chip: Typedtree.chip) =
  let udgi = mk_graph_chip t_chip in
  let g = udgi.graph in
  let leaf_vars (g: UDGraph.t) =
    let leaf_vars_fold (g: UDGraph.t) (v: UDGraphV.t) vl =
      let in_d = UDGraph.in_degree g v in
      let out_d = UDGraph.out_degree g v in
      match v.catg with
      | Input ->
        if out_d = 0 then v::vl
        else vl
      | Output ->
        if in_d = 0 then raise (Error (UnassignedOutVariable v.name))
        else vl
      | CurrentState ->
        if out_d = 0 then v::vl
        else vl
      | NextState ->
        if in_d = 0 then raise (Error (UnupdatedStateVariable v.name))
        else vl
      | Internal ->
        if out_d = 0 then v::vl
        else vl
    in
    UDGraph.fold_vertex (leaf_vars_fold g) g []
  in

  let remove_vertices (g: UDGraph.t) vs =
    List.fold_left (fun acc v -> UDGraph.remove_vertex acc v) g vs
  in

  let vars_to_warnings (vs: UDGraphV.t list) =
    let var_to_warning (v: UDGraphV.t) =
      match v.catg with
      | Input -> UnusedInVariable v.name
      | CurrentState -> UnusedStateVariable v.name
      | Internal -> UnusedInternalVariable v.name
      | Output | NextState -> raise (UnreachablePath "unreachable path")
    in
    List.map var_to_warning vs
  in

  let rec remove_unused_vars (g_wl: UDGraph.t * warning list) =
    let g = fst g_wl in
    let wl = snd g_wl in
    let vars = leaf_vars g in
    match vars with
    | [] -> g, wl
    | _ ->
      let wl' = vars_to_warnings vars in
      let wl' = wl' @ wl in
      let g' = remove_vertices g vars in
      remove_unused_vars (g', wl')
  in

  let g', wl = remove_unused_vars (g, []) in
  let () = List.iter report_warning wl in
  { udgi with graph=g' }

let ud_check_program (t_program: Typedtree.program) =
  try
    List.map
      ud_check_chip
      t_program
  with
  | Error e ->
    report_error e;
    exit 1
  | ConfigError ce ->
    report_config_error ce;
    exit 2
