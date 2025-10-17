open Location

module StrMap = Map.Make(String)

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
