open Patterns

module StrMap = Map.Make(String)

type expr =
  | Unit
  | Int     of int
  | String  of string
  | Bool    of bool
  | Id      of string
  | Let     of let_bind * expr
  | Tuple   of expr list
  | Object  of expr StrMap.t
  | Array   of expr list
  | Apply   of expr * expr list
  | ObjMem  of expr * string

and let_bind = pat * expr


let eunit = Unit
let eint n = Int n
let estring s = String s
let ebool b = Bool b
let eid id = Id id
let elet pat e eb = Let ((pat, e), eb)
let etuple el = Tuple el
let eobject er = Object er
let earray el = Array el
let eapply e el = Apply (e, el)
let eobjmem obj fld = ObjMem (obj, fld)

let blet (p : pat) (e : expr) : let_bind =
  (p, e)


module Pretty = struct
  let rec pp_expr ppf e =
    match e with
    | Unit -> Fmt.pf ppf "()"
    | Int n -> Fmt.int ppf n
    | String s -> Fmt.string ppf s
    | Bool b -> Fmt.bool ppf b
    | Id id -> Fmt.string ppf id
    | Let (lb, e) -> Fmt.pf ppf "let %a in %a"
                       pp_blet lb
                       pp_expr e
    | Tuple el -> Fmt.(parens (list ~sep:comma pp_expr)) ppf el
    | Object er -> Fmt.pf ppf "object %a end"
                     Fmt.(list ~sep:semi pp_method) (StrMap.to_list er)
    | Array el  -> Fmt.pf ppf "[| %a |]"
                     Fmt.(list ~sep:(any "; ") pp_expr) el
    | Apply (e, el) -> Fmt.pf ppf "%a %a"
                         pp_expr e
                         Fmt.(list ~sep:sp pp_expr) el
    | ObjMem (o, f) -> Fmt.pf ppf "(%a)#%s"
                         pp_expr o
                         f

  and pp_blet ppf (pat, e) =
    Fmt.pf ppf "%a = %a"
      Patterns.Pretty.pp_pat pat
      pp_expr e

  and pp_method ppf (id, e) =
    Fmt.pf ppf "method %s = %a"
      id
      pp_expr e
end
