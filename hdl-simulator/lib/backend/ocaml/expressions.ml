open Patterns
open Types

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
  | ModPath of string * string

and let_bind =
  | Simple  of pat * expr
  | FunDecl of string * (pat * typ option) list * typ option * expr


let eunit = Unit
let eint n = Int n
let estring s = String s
let ebool b = Bool b
let eid id = Id id
let elet pat e eb = Let (Simple(pat, e), eb)
let etuple el = Tuple el
let eobject er = Object er
let earray el = Array el
let eapply e el = Apply (e, el)
let eobjmem obj fld = ObjMem (obj, fld)
let modpath m f = ModPath (m, f)

let blet p e = Simple (p, e)
let bfun id ptl rt e = FunDecl (id, ptl, rt, e)


module Pretty = struct
  let colon ppf = Fmt.any ":@ " ppf

  let rec pp_expr ppf e =
    match e with
    | Unit -> Fmt.pf ppf "()"
    | Int n -> Fmt.int ppf n
    | String s -> Fmt.string ppf s
    | Bool b -> Fmt.bool ppf b
    | Id id -> Fmt.string ppf id
    | Let (lb, e) -> Fmt.pf ppf "%a in %a"
                       pp_blet lb
                       pp_expr e
    | Tuple el -> Fmt.(parens (list ~sep:comma pp_expr)) ppf el
    | Object er -> Fmt.pf ppf "object %a end"
                     Fmt.(list ~sep:sp pp_method) (StrMap.to_list er)
    | Array el  -> Fmt.pf ppf "[| %a |]"
                     Fmt.(list ~sep:(any "; ") pp_expr) el
    | Apply (e, el) -> Fmt.pf ppf "%a %a"
                         pp_expr e
                         Fmt.(list ~sep:sp pp_expr) el
    | ObjMem (o, f) -> Fmt.pf ppf "(%a)#%s"
                         pp_expr o
                         f
    | ModPath (m, f) -> Fmt.pf ppf "%s.%s" m f

  and pp_blet ppf lb =
    match lb with
    | Simple (p, e) ->
      Fmt.pf ppf "let %a = %a"
        Patterns.Pretty.pp_pat p
        pp_expr e

    | FunDecl (id, ptl, rt, e) ->
      Fmt.pf ppf "let %s %a %a =@ %a"
        id
        Fmt.(list ~sep:sp pp_ptl) ptl
        Fmt.(option (colon ++ Types.Pretty.pp_typ)) rt
        pp_expr e

  and pp_method ppf (id, e) =
    Fmt.pf ppf "method %s = %a"
      id
      pp_expr e

  and pp_ptl ppf (p, topt) =
    match topt with
    | Some t -> Fmt.pf ppf "(%a : %a)"
                  Patterns.Pretty.pp_pat p
                  Types.Pretty.pp_typ t
    | None   -> Fmt.pf ppf "(%a)"
                  Patterns.Pretty.pp_pat p
end
