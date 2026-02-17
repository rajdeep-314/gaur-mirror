open Types
open Hdlutils

type ('a, 'f) info =
  { payl : 'a
  ; info : 'f
  }

type 'i expr =
  | Const     of Bit.t NonEmpty.t
  | Apply     of 'i * 'i expr
  | Splice    of 'i expr * int * int
  | Concat    of 'i expr * 'i expr
  | Record    of 'i expr StrMap.t
  | Access    of string
  | Member    of 'i expr * string

type ('i, 'f) expr_info   = ('i expr, 'f) info

type ('i, 'f) assign =
  | Current   of 'i * ('i, 'f) expr_info
  | Next      of 'i * ('i, 'f) expr_info

type ('i, 'f) assign_info = (('i, 'f) assign, 'f) info

type 'i port =
  { name : 'i
  ; typ  : t
  }

type states = t StrMap.t

type ('i, 'f) chip =
  { name    : 'i
  ; input   : 'i port
  ; output  : 'i port
  ; states  : states option
  ; parts   : ('i, 'f) assign_info list
  }

module Pretty = struct
  let pp_dump_port : string port Fmt.t = fun ppf p ->
    Fmt.pf ppf "%s: %a@"
      p.name
      Pretty.pp_dump_t p.typ

  let pp_dump_states : states Fmt.t = fun ppf ss ->
    Fmt.pf ppf "%a@"
      (Fmt.list ~sep:Fmt.comma Pretty.pp_dump_f)
      (StrMap.to_list ss)

  let rec pp_dump_f : (string * string expr) Fmt.t =
    fun ppf f ->
    Fmt.pair ~sep:(Fmt.any ":@ ") Fmt.string pp_dump_expr
      ppf
      f

  and pp_dump_expr : string expr Fmt.t = fun ppf e ->
    match e with
    | Const b           -> Fmt.int ppf (Bitvec.to_int b)

    | Apply (id, e)     -> Fmt.pf ppf "%s %a" id pp_dump_expr e

    | Splice (e, i, j)  -> Fmt.pf ppf "%a[%d:%d]"
                             pp_dump_expr e
                             i
                             j

    | Concat (e1, e2)   -> Fmt.pf ppf "%a@++@%a"
                             pp_dump_expr e1
                             pp_dump_expr e2

    | Record r          -> Fmt.braces
                             (Fmt.list ~sep:Fmt.comma pp_dump_f)
                             ppf
                             (StrMap.to_list r)

    | Access id         -> Fmt.string ppf id

    | Member (e, id)    -> Fmt.pf ppf "%a.%s" pp_dump_expr e id

  let pp_dump_assign : (string, unit) assign Fmt.t =
    fun ppf a ->
    match a with
    | Current (id, expr) -> Fmt.pf ppf "%s' <- %a@" id pp_dump_expr expr.payl
    | Next    (id, expr) -> Fmt.pf ppf "%s := %a@" id pp_dump_expr expr.payl

  let pp_dump_assign_info : (string, unit) assign_info Fmt.t =
    fun ppf ai -> pp_dump_assign ppf ai.payl
end
