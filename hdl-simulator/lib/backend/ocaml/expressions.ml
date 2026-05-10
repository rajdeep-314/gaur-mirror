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
  | Fun     of (pat * typ option) list * typ option * expr
  | List    of expr list
  | Infix   of string * expr * expr
  | Match   of expr * (pat * expr) list
  | Module  of string
  | Variant of string * expr list

and let_bind =
  | Simple  of pat * expr
  | FunDecl of string * (pat * typ option) list * typ option * expr


let eunit = Unit
let eint n = Int n
let estring s = String s
let ebool b = Bool b
let efalse = ebool false
let etrue  = ebool true
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
let efun ptl rt e = Fun (ptl, rt, e)
let elist el = List el
let einfix id el er = Infix (id, el, er)
let ematch scr pel = Match (scr, pel)
let emodule m = Module m
let evar v el = Variant (v, el)

let blet p e = Simple (p, e)
let bfun id ptl rt e = FunDecl (id, ptl, rt, e)


module Pretty = struct
  let colon ppf = Fmt.any ": " ppf
  let semi ppf = Fmt.any "; " ppf

  let rec pp_expr ppf e =
    match e with
    | Unit           -> Fmt.pf ppf "()"
    | Int n          -> Fmt.int ppf n
    | String s       -> Fmt.quote Fmt.string ppf s
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
    | Fun (ptl, rt, e) -> Fmt.pf ppf "@[<v 2>fun %a %a ->@ %a@]"
                            Fmt.(list ~sep:sp pp_ptl) ptl
                            Fmt.(option (colon ++ Types.Pretty.pp_typ)) rt
                            pp_expr e
    | List el          -> Fmt.pf ppf "[ %a ]"
                            (Fmt.list ~sep:semi pp_expr) el
    | Infix (id, el, er) -> Fmt.pf ppf "(%a) %s (%a)"
                              pp_expr el
                              id
                              pp_expr er
    | Match (scr, pel)   -> Fmt.pf ppf "@[<v 2>match %a with@,%a@,@]"
                              pp_expr scr
                              (Fmt.list pp_match_clause) pel
    | Module m           -> Fmt.pf ppf "(module %s)" m
    | Variant (v, el)    -> Fmt.pf ppf "%s %a"
                              (String.capitalize_ascii v)
                              Fmt.(parens (list ~sep:comma pp_expr)) el

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

  and pp_match_clause ppf (pat, e) =
    Fmt.pf ppf "| %a -> %a"
      Patterns.Pretty.pp_pat pat
      pp_expr e
end
