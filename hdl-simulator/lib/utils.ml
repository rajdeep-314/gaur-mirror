module StrMap = Map.Make(String)

module StrSet = Set.Make(String)

let has_dupe xs x =
  match List.find_opt (String.equal x) xs with
  | None -> false
  | Some _ -> true

exception UnreachablePath of string
