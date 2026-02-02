open Parse.Asttypes
open Parse
open Hdlutils

type error =
  (* Type Errors *)
  | Splice_non_bits of typ
  | Splice_index_out_of_range of int * int * int
  | Concat_non_bits of typ * typ
  | Not_a_chip of ident (* This exception should not be raised under the current design. *)
  | Chip_argument_type_mismatch of ident * (typ * typ) * typ
  | Assignment_type_clash of typ * typ
  | Member_access_non_record of typ
  | Unbound_record_field of ident

  (* Lookup Errors *)
  | Unbound_variable of ident
  | Unbound_chip of ident

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

  | Unbound_record_field field ->
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

  | Unbound_chip chip ->
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


let rec infer_or_raise env (p_expr:Parsetree.expr) =
  let loc = p_expr.expr_loc in
  match p_expr.expr_desc with
  | Pexpr_constant b ->
    let l  = NonEmpty.length b in
    (Typedtree.mk_expr ~loc:loc (Texpr_constant b) (TBits l) env)

  | Pexpr_apply (chip, arg) ->
    let typed_arg = infer_or_raise env arg in
    let typ_arg = typed_arg.expr_typ in
    let typ_chip_opt =  Env.lookup chip env in
    begin match typ_chip_opt with
      | None -> raise (Error (loc, env, Unbound_chip chip))
      | Some (TBits _)
      | Some (TRecord _) -> raise (Error (loc, env, Not_a_chip chip))
      | Some (TChip (typ_in, typ_out)) ->
        if typ_equal typ_in typ_arg
        then Typedtree.mk_expr ~loc:loc (Texpr_apply (chip, typed_arg)) typ_out env
        else raise (Error (loc, env, Chip_argument_type_mismatch (chip, (typ_in, typ_out), typ_arg)))
    end

  | Pexpr_splice (expr, start_index, end_index) ->
    let typed_expr = infer_or_raise env expr in
    let typ_expr = typed_expr.expr_typ in
    begin match typ_expr with
      | TRecord _
      | TChip _ -> raise (Error (loc, env, Splice_non_bits typ_expr))
      | TBits n ->
        if (0 <= start_index) && (start_index <= end_index) && (end_index < n)
        then Typedtree.mk_expr ~loc:loc (Texpr_splice (typed_expr, start_index, end_index)) (TBits (end_index - start_index + 1)) env
        else raise (Error (loc, env, Splice_index_out_of_range (start_index, end_index, n)))
    end

  | Pexpr_concat (expr1, expr2) ->
    let typed_expr1 = infer_or_raise env expr1 in
    let typ_expr1 = typed_expr1.expr_typ in
    let typed_expr2 = infer_or_raise env expr2 in
    let typ_expr2 = typed_expr2.expr_typ in
    begin match typ_expr1, typ_expr2 with
      | TBits n, TBits m -> Typedtree.mk_expr ~loc:loc (Texpr_concat (typed_expr1, typed_expr2)) (TBits (n + m)) env
      | _ -> raise (Error (loc, env, Concat_non_bits (typ_expr1, typ_expr2)))
    end

  | Pexpr_record record ->
    let typed_record = StrMap.map (infer_or_raise env) record in
    let typs = StrMap.map (fun x -> x.Typedtree.expr_typ) typed_record in
    Typedtree.mk_expr ~loc:loc (Texpr_record typed_record) (TRecord typs) env

  | Pexpr_access var ->
    let typ_var_opt = Env.lookup var env in
    begin match typ_var_opt with
      | None -> raise (Error (loc, env, Unbound_variable var))
      | Some typ_var -> Typedtree.mk_expr ~loc:loc (Texpr_access var) typ_var env
    end

  | Pexpr_member (expr, field) ->
    let typed_expr = infer_or_raise env expr in
    let typ_expr = typed_expr.expr_typ in
    begin match typ_expr with
      | TBits _
      | TChip _ -> raise (Error (loc, env, Member_access_non_record typ_expr))
      | TRecord r ->
        let typ_field_opt = StrMap.find_opt field r in
        begin match typ_field_opt with
          | None -> raise (Error (loc, env, Unbound_record_field field))
          | Some typ_field -> Typedtree.mk_expr ~loc:loc (Texpr_member (typed_expr, field)) typ_field env
        end
    end


let tc_assignment env (p_assign:Parsetree.assign) =
  let loc = p_assign.assign_loc in
  let desc = p_assign.assign_desc in
  let state = p_assign.assign_desc.state in
  let typed_expr = infer_or_raise env desc.expr in
  let typ_expr = typed_expr.expr_typ in
  let typ_var_opt = Env.lookup desc.var env in
  let assign_desc = Typedtree.mk_assign_desc desc.var typed_expr state in
  match typ_var_opt with
  | None ->
    let env' = Env.extend desc.var typ_expr env in
    let assign = Typedtree.mk_assign ~loc:loc assign_desc env' in
    assign
  (* | None -> Typedtree.mk_assign ~loc:loc var desc (Env.extend desc.var typ_expr env) *)
  | Some typ_var ->
    if typ_equal typ_var typ_expr
    then Typedtree.mk_assign ~loc:loc assign_desc env
    else raise (Error (loc, env, Assignment_type_clash (typ_var, typ_expr)))


let extend_ports env (p_chip:Parsetree.chip) =
  env
  |> Env.extend p_chip.input.port_name p_chip.input.port_type
  |> Env.extend p_chip.output.port_name p_chip.output.port_type


let extend_states env (chip:Parsetree.chip) =
  match chip.states with
  | None -> env
  | Some ss -> StrMap.fold Env.extend ss env


let tc_chip env chip =
  let env' = extend_ports env chip in
  let env'' = extend_states env' chip in

  let rec fold env p_chip =
    match p_chip with
    | [] -> [], env
    | [p_assign] ->
      let typed_assign = tc_assignment env p_assign in
      let env' = typed_assign.assign_env in
      [typed_assign], env'
    | p_assign::rest ->
      let typed_assign = tc_assignment env p_assign in
      let env' = typed_assign.assign_env in
      let folded_rest = fold env' rest in
      (typed_assign :: (fst folded_rest), snd folded_rest)
  in
  let typed_assign = fold env'' chip.parts in
  let parts = fst typed_assign in
  let env = snd typed_assign in
  begin
    Printf.printf "\nEnvironment of chip %s:\n" chip.name;
    Env.print env;
    Typedtree.{ name=chip.name;
                input=chip.input;
                output=chip.output;
                states=chip.states;
                parts=parts;
                env=env
              }
  end


let tc_program env p_program =
  try
    List.map
      (tc_chip env)
      p_program
  with Error (loc, _, error) ->
    report_error ~loc:loc error;
    exit 1
