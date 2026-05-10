module N = Name

open Parse

module StrMap = Map.Make(String)

let rec typ (t : Types.t) : Types.t =
  match t with
  | TBits   n           -> TBits n
  | TRecord tr          -> TRecord (typ_record tr)
  | TChip   (tin, tout) -> TChip (typ tin, typ tout)

  and typ_record tr =
    StrMap.to_list tr
    |> List.map (fun (fld, t) -> Name.var_current fld, typ t)
    |> StrMap.of_list

let rec expr (ei : 'a Ast.expr_info) : 'a Ast.expr_info =
  let e : 'a Ast.expr =
    match ei.data with
    | Const  bv           -> Const bv
    | Record r            -> Record (expr_record r)
    | Splice (e', lb, ub) -> Splice (expr e', lb, ub)
    | Concat (e1, e2)     -> Concat (expr e1, expr e2)
    | Access id           -> Access (N.var_current id)
    | Member (e', fld)    -> Member (expr e', N.var_current fld)
  in
  { ei with data = e }

  and expr_record r =
    StrMap.to_list r
    |> List.map (fun (fld, e) -> Name.var_current fld, expr e)
    |> StrMap.of_list

let rhs (ri : 'a Ast.rhs_info) : 'a Ast.rhs_info =
  let r : 'a Ast.rhs =
    match ri.data with
    | Call (id, e) -> Call (N.module_name id, expr e)
    | Expr e       -> Expr (expr e)
  in
  { ri with data = r }

let assign (ai : 'a Ast.assign_info) : 'a Ast.assign_info =
  let a : 'a Ast.assign =
    match ai.data with
    | Current (id, r) -> Current (N.var_current id, rhs r)
    | Next    (id, r) -> Next    (N.var_next    id, rhs r)
  in
  { ai with data = a }

let parts (pi : 'a Ast.assign_info list) : 'a Ast.assign_info list =
  List.map assign pi

let input (ip : Ast.port) : Ast.port =
  { name=N.var_current ip.name
  ; typ=typ ip.typ
  }

let output (op : Ast.port) : Ast.port =
  { name=N.var_current op.name
  ; typ=typ op.typ
  }

let states (sr : Types.t StrMap.t) : Types.t StrMap.t =
  StrMap.to_list sr
  |> List.map (fun (id, t) -> (N.var_current id, t))
  |> StrMap.of_list

let chip (ci : 'a Ast.chip_info) : 'a Ast.chip_info =
  let c = ci.data in
  let c' : 'a Ast.chip =
    { name=N.module_name c.name
    ; input=input c.input
    ; output=output c.output
    ; states=states c.states
    ; parts=parts c.parts
    }
  in
  { ci with data = c' }

let prog (p : 'a Ast.prog) : 'a Ast.prog =
  List.map chip p
