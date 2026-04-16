type pat =
  | Id of string


let pid id = Id id


module Pretty = struct
  let pp_pat ppf p =
    match p with
    | Id id -> Fmt.string ppf id
end
