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
  | Record  of expr StrMap.t
  | RecMem  of expr * string

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
let erecord er = Record er
let erecmem r fld = RecMem (r, fld)

let blet p e = Simple (p, e)
let bfun id ptl rt e = FunDecl (id, ptl, rt, e)


module Pretty = struct
  let colon ppf = Fmt.any ": " ppf

  let rec pp_expr ppf e =
    match e with
    | Unit           -> Fmt.pf ppf "()"
    | Int n          -> Fmt.int ppf n
    | String s       -> Fmt.string ppf s
    | Bool b         -> Fmt.bool ppf b
    | Id id          -> Fmt.string ppf id
    | Let (lb, e)    -> Fmt.pf ppf "@[<v>%a@,in@,%a@]"
                          pp_blet lb
                          pp_expr e
    | Tuple el       -> Fmt.(parens (list ~sep:comma pp_expr)) ppf el
    | Object er      -> Fmt.pf ppf "@[<v>object@;<0 2>@[<v>%a@]@,end@]"
                          Fmt.(list pp_method) (StrMap.to_list er)
    | Array el       -> Fmt.pf ppf "[| %a |]"
                          Fmt.(list ~sep:(any "; ") pp_expr) el
    | Apply (e, el)  -> Fmt.pf ppf "@[<v 2>%a@,%a@]"
                          pp_expr e
                          Fmt.(list ~sep:sp (parens pp_expr)) el
    | ObjMem (o, f)  -> Fmt.pf ppf "(%a)#%s"
                          pp_expr o
                          f
    | ModPath (m, f) -> Fmt.pf ppf "%s.%s" m f
    | Record er      -> Fmt.pf ppf "@[<v>{@;<0 2>@[<v>%a@]@,}@]"
                          Fmt.(list ~sep:semi pp_field) (StrMap.to_list er)
    | RecMem (r, f)  -> Fmt.pf ppf "(%a).%s"
                          pp_expr r
                          f

  and pp_blet ppf lb =
    match lb with
    | Simple (p, e) ->
      Fmt.pf ppf "@[<v 2>let %a =@,%a@]"
        Patterns.Pretty.pp_pat p
        pp_expr e

    | FunDecl (id, ptl, rt, e) ->
      Fmt.pf ppf "@[<v 2>let %s %a %a =@ %a@]"
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

  and pp_field ppf (id, e) =
    Fmt.pf ppf "%s = %a"
      id
      pp_expr e
end
