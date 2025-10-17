module StrMap = Map.Make(String)

let ( let* ) = Result.bind

exception DuplicateKey of string * (Lexing.position * Lexing.position)

let add_to_map map key loc value =
  if StrMap.mem key map then
    raise (DuplicateKey (key, loc))
  else
    StrMap.add key value map
