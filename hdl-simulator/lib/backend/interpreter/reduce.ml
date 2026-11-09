(** Reduction from [Parse.Ast] to the interpreter's {!Ast}. *)

module ParseAst  = Parse.Ast
module ParseType = Parse.Types
module StrMap    = Ast.StrMap

let rec to_tipe (t : ParseType.t) : Ast.tipe =
  match t with
  | TBits n   -> TBitvec n
  | TRecord r -> TRecord (StrMap.map to_tipe r)
  | TChip _   -> invalid_arg "Reduce.to_tipe: a chip type is not a value type"
  (* TODO Change parse AST to remove this unwanted exception *)

let rec reduce_expr (kind : string -> Ast.var) (ei : 'a ParseAst.expr_info) : Ast.expr =
  match ei.data with
  | Const bv           -> Const bv
  | Record r           -> Record (StrMap.map (reduce_expr kind) r)
  | Splice (e, lb, ub) -> Splice (reduce_expr kind e, lb, ub)
  | Concat (e1, e2)    -> Concat (reduce_expr kind e1, reduce_expr kind e2)
  | Access var         -> Access (kind var, var)
  | Member (e, f)      -> Member (reduce_expr kind e, f)

(** [id] is the instance id given to a chip call and is unique within a chip. *)
let reduce_rhs (id : int) kind (ri : 'a ParseAst.rhs_info) : Ast.rhs =
  match ri.data with
  | Call (chip, e) -> Call (chip, id, reduce_expr kind e)
  | Expr e         -> Expr (reduce_expr kind e)

let reduce_assign (id : int) kind (ai : 'a ParseAst.assign_info) : Ast.assign =
  match ai.data with
  | Current (var, r) -> Current (var, reduce_rhs id kind r)
  | Next    (var, r) -> Next    (var, reduce_rhs id kind r)

let reduce_parts kind parts =
  List.mapi (fun id ai -> reduce_assign id kind ai) parts

(** [classify chip var] is the kind of the variable [var] in [chip]. *)
let classify (chip : 'a ParseAst.chip) (var : string) : Ast.var =
  if String.equal var chip.input.name then Input
  else if StrMap.mem var chip.states then State
  else Inter

let reduce_chip (ci : 'a ParseAst.chip_info) : Ast.chip =
  let chip = ci.data in
  { name=chip.name
  ; input=
      { name=chip.input.name
      ; tipe=to_tipe chip.input.typ
      }
  ; output=
      { name=chip.output.name
      ; tipe=to_tipe chip.output.typ
      }
  ; states=StrMap.map to_tipe chip.states
  ; parts=reduce_parts (classify chip) chip.parts
  }

let reduce_prog (p : 'a ParseAst.prog) : Ast.prog =
  List.map reduce_chip p
