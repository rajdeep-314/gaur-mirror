type pat =
  | Id     of string
  | String of string


let pid id = Id id
let pwild = Id "_"
let pstring s = String s


module Pretty = struct
  let pp_pat ppf p =
    match p with
    | Id     id -> Fmt.string ppf id
    | String s  -> Fmt.quote Fmt.string ppf s
end
