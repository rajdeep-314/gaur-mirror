open Ast

type error = 
  | Splice_non_bits of typ
  | Splice_index_out_of_range of int * int * int
  | Concat_non_bits of typ * typ
  | Unknown_record_field of ident
  | Member_access_non_record of typ
  | Unbound_variable of ident
  | Not_a_chip of ident (* This exception should not be raised under the current design. *)
  | Unknown_chip of ident
  | Chip_argument_type_mismatch of ident * (typ * typ) * typ
  | Multiple_bound_variable of ident
  | Current_assignment_state_var of ident
  | Next_assignment_non_state_var of ident

let report_error ~loc = function
  | Splice_non_bits typ -> 
    Location.errorf ~loc:loc
      "Can not splice an expression of type %s.\n"
      (Pretty.pp_typ typ)

  | Splice_index_out_of_range (start_index, end_index, width) ->
    Location.errorf ~loc:loc
      "Splicing indices: [%d, %d] are out of range: [0, %d].\n"
      start_index end_index (width - 1)

  | Concat_non_bits (typ_expr1, typ_expr2) ->
    Location.errorf ~loc:loc
      "Can not concat expressions of type %s and %s.\n"
      (Pretty.pp_typ typ_expr1) (Pretty.pp_typ typ_expr2)

  | Unknown_record_field field ->
    Location.errorf ~loc:loc
      "This expression has no field named %s.\n"
      field

  | Member_access_non_record typ ->
    Location.errorf ~loc:loc
      "Can not access members of expression of type %s.\n"
      (Pretty.pp_typ typ)

  | Unbound_variable var ->
    Location.errorf ~loc:loc
      "Unbound variable %s.\n"
      var

  | Not_a_chip _ ->
    (* This exception should not be raised under the current design. *)
    Location.errorf ~loc:loc
      "[Bug] [Not_a_chip error] How did this exception got raised ?\n"

  | Unknown_chip chip ->
    Location.errorf ~loc:loc
      "Unknown chip %s.\n"
      chip

  | Chip_argument_type_mismatch (chip, (t_in, _), t_arg) ->
    Location.errorf ~loc:loc
      "The chip %s expects an expression of type: %s.\n\
      \ But an expression was passed of type: %s.\n\
      \ The type %s is not compatible with type %s.\n"
      chip
      (Pretty.pp_typ t_in)
      (Pretty.pp_typ t_arg)
      (Pretty.pp_typ t_in)
      (Pretty.pp_typ t_arg)

  | Multiple_bound_variable var ->
    Location.errorf ~loc:loc
      "The variable %s is bound multiple times.\n"
      var

  | Current_assignment_state_var var ->
    Location.errorf ~loc:loc
      "Can not combinationally assign the state variable %s.\n"
      var

  | Next_assignment_non_state_var var ->
    Location.errorf ~loc:loc
      "Can not sequentially assign the non-state variable %s.\n"
      var


exception Error of Location.loc * Env.t * error

let rec infer env expr = 
  match expr.expr_desc with
  | Constant b ->
    let l = NonEmpty.length b in
    (TBits l)

  | Instance (chip, expr_arg) ->
    begin match Env.lookup chip env with  
      | None -> raise (Error (expr.expr_loc, env, Unknown_chip chip))
      | Some (TBits _)
      | Some (TRecord _) -> raise (Error (expr.expr_loc, env, Not_a_chip chip))
      | Some (TChip (typ_in, typ_out)) -> 
        let expr_arg_typ = infer env expr_arg in 
        if typ_equal expr_arg_typ typ_in
        then typ_out
        else raise (Error (expr_arg.expr_loc, env, Chip_argument_type_mismatch (chip, (typ_in, typ_out), expr_arg_typ)))
    end

  | Splice (splice_expr, start_index, end_index) ->
    let typ_splice_expr = infer env splice_expr in
    begin match typ_splice_expr with
      | TBits n -> 
        if (0 <= start_index) && (start_index <= end_index) && (end_index < n)
        then TBits (end_index - start_index + 1)
        else raise (Error (expr.expr_loc, env, Splice_index_out_of_range (start_index, end_index, n)))
      | _ -> raise (Error (expr.expr_loc, env, Splice_non_bits (typ_splice_expr)))
    end

  | Concat (expr1, expr2) ->
    let typ_expr1 = infer env expr1 in
    let typ_expr2 = infer env expr2 in
    begin match typ_expr1, typ_expr2 with
      | TBits n, TBits m -> TBits (n + m)
      | _ -> raise (Error (expr.expr_loc, env, Concat_non_bits (typ_expr1, typ_expr2)))
    end

  | Record expr_record ->
    let typs = StrMap.map (infer env) expr_record in
    TRecord typs

  | Access ident ->
    begin match Env.lookup ident env with
      | Some t -> t
      | None -> raise (Error (expr.expr_loc, env, Unbound_variable ident))
    end

  | Member (expr_mem, field) ->
    let typ_expr_mem = infer env expr_mem in
    begin match typ_expr_mem with
      | TRecord r -> 
        begin match StrMap.find_opt field r with
          | Some t -> t
          | None -> raise (Error (expr.expr_loc, env, Unknown_record_field (field)))
        end
      | _ -> raise (Error (expr.expr_loc, env, Member_access_non_record (typ_expr_mem)))
    end


(** [is_state_var c s] checks if variable [v] is a state variable in the map [s] *)
let is_state_var var states = 
  match states with
  | None -> false
  | Some ss -> StrMap.mem var ss


(** [type_check_assignment env states assign] checks if the new assignment
    [assign] is safe under environment [env] by assuring that a stateful
    assignment is done if and only if it is a state variable, and that
    no variable is assigned twice.
*)
let type_check_assignment states env assignment = 
  let var = assignment.assign_desc.var in
  let expr = assignment.assign_desc.expr in
  let state = assignment.assign_desc.state in
  match state with
  | Current -> 
    if is_state_var var states
    then raise (Error (assignment.assign_loc, env, Current_assignment_state_var var))
    else 
      let expr_typ = infer env expr in
      Env.extend var expr_typ env

  | Next -> 
    if not (is_state_var var states)
    then raise (Error (assignment.assign_loc, env, Next_assignment_non_state_var var))
    else
    if Env.mem var env
    then raise (Error (assignment.assign_loc, env, Multiple_bound_variable var))
    else 
      let expr_typ = infer env expr in
      Env.extend var expr_typ env


let extend_ports chip env = 
  env
  |> Env.extend chip.input.port_name chip.input.port_type
  |> Env.extend chip.output.port_name chip.output.port_type

let extend_states chip env = 
  match chip.states with
  | None -> env
  | Some ss -> Env.fold Env.extend ss env

let type_check_chip env chip = 
  let env' = 
    env
    |> extend_ports chip
    |> extend_states chip
  in

  List.fold_left
    (type_check_assignment chip.states)
    env'
    chip.parts


let type_check_program env program = 
  try
    List.fold_left
      type_check_chip
      env
      program
  with Error (loc, _, error) ->
    report_error ~loc:loc error;
    exit 1
