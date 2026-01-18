open Location

module StrMap = Map.Make(String)

module StrSet = Set.Make(String)

let ( let* ) = Result.bind

exception DuplicateKey of string * Location.loc

let add_to_map map kvl =
  let key = fst (kvl.txt) in
  let value = snd (kvl.txt) in
  let loc = kvl.loc in

  if StrMap.mem key map then
    raise (DuplicateKey (key, loc))
  else
    StrMap.add key value map


let has_dupe xs x =
  match List.find_opt (String.equal x) xs with
  | None -> false
  | Some _ -> true


exception UnreachablePath of string
