open Ast

module StrMap = Map.Make(String)

type t = typ StrMap.t

let lookup = StrMap.find_opt

let extend = StrMap.add

let empty_env = StrMap.empty

let mem = StrMap.mem

let fold = StrMap.fold
