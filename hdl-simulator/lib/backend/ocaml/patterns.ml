type pat =
  | Id      of string
  | Int     of int
  | String  of string
  | Module  of string
  | Variant of string * pat list


let pid id = Id id
let pint n = Int n
let pwild = Id "_"
let pstring s = String s
let pmodule m = Module m
let pvariant s pl = Variant (s, pl)


module Pretty = struct
  let rec pp_pat ppf p =
    match p with
    | Id      id      -> Fmt.string ppf id
    | Int     n       -> Fmt.int ppf n
    | String  s       -> Fmt.quote Fmt.string ppf s
    | Module  m       -> Fmt.pf ppf "(module %s)"
                           (String.capitalize_ascii m)
    | Variant (s, pl) -> Fmt.pf ppf "%s %a"
                           s
                           pp_var_args pl
  and pp_var_args ppf pl =
    match pl with
    | [] -> Fmt.nop ppf ()
    | _  -> Fmt.(parens (list ~sep:comma pp_pat)) ppf pl
end
