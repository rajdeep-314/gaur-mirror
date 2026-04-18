module StrMap = Map.Make(String)

type typ =
  | Unit
  | Int
  | String
  | Bool
  | Id      of string
  | Arrow   of typ * typ
  | Tuple   of typ list
  | Object  of typ StrMap.t
  | Array   of typ
  | ModPath of string * string

and typ_decl = string * typ_info

and typ_info =
  | Typ     of typ
  | Record  of typ StrMap.t
  | Variant of typ list StrMap.t


let tunit = Unit
let tint = Int
let tstring = String
let tbool = Bool
let tid id = Id id
let tarrow tin tout = Arrow (tin, tout)
let ttuple tl = Tuple tl
let tobject tr = Object tr
let tarray t = Array t
let tmodpath m t = ModPath (m, t)

let decl_alias   lid typ : typ_decl = (lid, Typ typ)
let decl_record  lid fl  : typ_decl = (lid, Record fl)
let decl_variant lid vl  : typ_decl = (lid, Variant vl)


module Pretty = struct
  let star ppf = Fmt.any "*" ppf
  let spstar ppf = Fmt.any " * " ppf
  let spsemi ppf = Fmt.any "@ ;@ " ppf

  let rec pp_typ ppf typ =
    match typ with
    | Unit              -> Fmt.pf ppf "unit"
    | Int               -> Fmt.pf ppf "int"
    | String            -> Fmt.pf ppf "string"
    | Bool              -> Fmt.pf ppf "bool"
    | Id lid            -> Fmt.pf ppf "%s" lid
    | Arrow (tin, tout) -> Fmt.pf ppf "(%a -> %a)"
                             pp_typ tin
                             pp_typ tout
    | Tuple tl          -> Fmt.parens (Fmt.list ~sep:spstar pp_typ) ppf tl
    | Object tr         -> Fmt.pf ppf "@[<h>< %a >@]"
                             (Fmt.list ~sep:spsemi pp_method)
                             (StrMap.to_list tr)
    | Array t           -> Fmt.pf ppf "@[<hv 2>%a@ array@]"
                             pp_typ t
    | ModPath (m, t)    -> Fmt.pf ppf "%s.%s" m t

  and pp_typ_decl ppf (lid, typ_info) =
    Fmt.pf ppf "@[<hv 2>type %s = @,%a@]"
      lid
      pp_typ_info typ_info

  and pp_typ_info ppf ti =
    match ti with
    | Typ t       -> pp_typ ppf t
    | Record fl   -> Fmt.braces
                       (Fmt.list ~sep:Fmt.semi pp_field_decl)
                       ppf
                       (StrMap.to_list fl)
    | Variant vl  -> Fmt.vbox (Fmt.list pp_var_decl) ppf (StrMap.to_list vl)

  and pp_field_decl ppf (lid, typ) =
    Fmt.pf ppf "%s : %a"
      lid
      pp_typ typ

  and pp_var_decl ppf (uid, tl) =
    Fmt.(pf ppf "| %s %a%a"
      uid
      (if' (not (List.is_empty tl)) Fmt.string) "of "
      (list ~sep:(spstar) pp_typ) tl
    )

  and pp_method ppf (lid, typ) =
    Fmt.pf ppf "%s : %a"
      lid
      pp_typ typ
end
