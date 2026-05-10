type pat =
  | Id      of string
  | String  of string
  | Variant of string * pat list


let pid id = Id id
let pwild = Id "_"
let pstring s = String s
let pvariant s pl = Variant (s, pl)


module Pretty = struct
  let rec pp_pat ppf p =
    match p with
    | Id      id      -> Fmt.string ppf id
    | String  s       -> Fmt.quote Fmt.string ppf s
    | Variant (s, pl) -> Fmt.pf ppf "%s (%a)"
                           s
                           Fmt.(list ~sep:comma pp_pat) pl
end
