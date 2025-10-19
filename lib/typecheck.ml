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
  | Assignment_type_clash of typ * typ

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
      \ But an expression was passed of type: %s.\n"
      chip
      (Pretty.pp_typ t_in)
      (Pretty.pp_typ t_arg)

  | Assignment_type_clash (typ_var, typ_expr) ->
    Location.errorf ~loc:loc
      "An expresison of type %s can not be assigned to a variable of type %s.\n"
      (Pretty.pp_typ typ_expr)
      (Pretty.pp_typ typ_var)


exception Error of Location.loc * Env.t * error


let rec infer_or_raise env expr =
  let loc = expr.expr_loc in
  match expr.expr_desc with
  | Constant b ->
    let l  = NonEmpty.length b in
    TBits l

  | Instance (chip, arg) ->
    let typ_arg = infer_or_raise env arg in
    let typ_chip_opt = Env.lookup chip env in
    begin match typ_chip_opt with
      | None -> raise (Error (loc, env, Unknown_chip chip))
      | Some (TBits _)
      | Some (TRecord _) -> raise (Error (loc, env, Not_a_chip chip))
      | Some (TChip (typ_in, typ_out)) ->
        if typ_equal typ_in typ_arg
        then typ_out
        else raise (Error (loc, env, Chip_argument_type_mismatch (chip, (typ_in, typ_out), typ_arg)))
    end

  | Splice (expr, start_index, end_index) ->
    let typ_expr = infer_or_raise env expr in
    begin match typ_expr with
      | TRecord _
      | TChip _ -> raise (Error (loc, env, Splice_non_bits typ_expr))
      | TBits n ->
        if (0 <= start_index) && (start_index <= end_index) && (end_index < n)
        then TBits (end_index - start_index + 1)
        else raise (Error (loc, env, Splice_index_out_of_range (start_index, end_index, n)))
    end

  | Concat (expr1, expr2) ->
    let typ_expr1 = infer_or_raise env expr1 in
    let typ_expr2 = infer_or_raise env expr2 in
    begin match typ_expr1, typ_expr2 with
      | TBits n, TBits m -> TBits (n + m)
      | _ -> raise (Error (loc, env, Concat_non_bits (typ_expr1, typ_expr2)))
    end

  | Record rec_expr ->
    let typs = StrMap.map (infer_or_raise env) rec_expr in
    TRecord typs

  | Access var ->
    let typ_var_opt = Env.lookup var env in
    begin match typ_var_opt with
      | None -> raise (Error (loc, env, Unbound_variable var))
      | Some typ_var -> typ_var
    end

  | Member (expr, field) ->
    let typ_expr = infer_or_raise env expr in
    begin match typ_expr with
      | TBits _
      | TChip _ -> raise (Error (loc, env, Member_access_non_record typ_expr))
      | TRecord r ->
        let typ_field_opt = StrMap.find_opt field r in
        begin match typ_field_opt with
          | None -> raise (Error (loc, env, Unknown_record_field field))
          | Some typ_field -> typ_field
        end
    end


let tc_assignment env assignment =
  let loc = assignment.assign_loc in
  let desc = assignment.assign_desc in
  let typ_expr = infer_or_raise env desc.expr in
  let typ_var_opt = Env.lookup desc.var env in
  match typ_var_opt with
  | None -> Env.extend desc.var typ_expr env
  | Some typ_var ->
    if typ_equal typ_var typ_expr
    then env
    else raise (Error (loc, env, Assignment_type_clash (typ_var, typ_expr)))


let extend_ports env chip =
  env
  |> Env.extend chip.input.port_name chip.input.port_type
  |> Env.extend chip.output.port_name chip.output.port_type


let extend_states env chip =
  match chip.states with
  | None -> env
  | Some ss -> StrMap.fold Env.extend ss env

let tc_chip env chip =
  let env' = extend_ports env chip in
  let env'' = extend_states env' chip in
  List.fold_left tc_assignment env'' chip.parts


let tc_program env program =
  try
    List.fold_left
      tc_chip
      env
      program
  with Error (loc, _, error) ->
    report_error ~loc:loc error;
    exit 1
