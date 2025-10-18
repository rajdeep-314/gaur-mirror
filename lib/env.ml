open Asttypes

module StrMap = Map.Make(String)

type t = symbol_info StrMap.t

let lookup = StrMap.find_opt

let extend = StrMap.add

let empty_env = StrMap.empty

let mem = StrMap.mem

let fold = StrMap.fold

let is_used id env =
  let info = StrMap.find id env in
  match info.used with
  | Used -> true
  | Unused -> false

let mark_used id env =
  let info = StrMap.find id env in
  info.used <- Used

let is_bound id env =
  let info = StrMap.find id env in
  match info.bound with
  | Bound -> true
  | Unbound -> false

let mark_bound id env =
  let info = StrMap.find id env in
  info.bound <- Bound

let is_updated id env =
  let info = StrMap.find id env in
  match info.updated with
  | Updated -> true
  | Unupdated -> false

let mark_updated id env =
  let info = StrMap.find id env in
  info.updated <- Updated
