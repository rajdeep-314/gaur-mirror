open Parse.Types

module StrMap = Map.Make(String)

type t = typ StrMap.t

let lookup = StrMap.find_opt

let extend = StrMap.add

let empty_env = StrMap.empty

let mem = StrMap.mem

let fold = StrMap.fold

let print =
  let print_pair s t =
    Printf.printf "%s: %s\n" s (Parse.Pretty.pp_typ t)
  in
  StrMap.iter print_pair;
