open Asttypes

module StrMap = Map.Make(String)

type t = typ StrMap.t

let lookup = StrMap.find_opt

let extend = StrMap.add

let empty_env = StrMap.empty

let mem = StrMap.mem

let fold = StrMap.fold

let print =
  let print_pair s t =
    begin
      Printf.printf "%s: %s\n"
        s
        (Pretty.pp_typ t)
    end
  in
  StrMap.iter print_pair;
