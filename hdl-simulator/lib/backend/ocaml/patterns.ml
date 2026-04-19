type pat =
  | Id of string


let pid id = Id id

let pwild = Id "_"


module Pretty = struct
  let pp_pat ppf p =
    match p with
    | Id id -> Fmt.string ppf id
end
