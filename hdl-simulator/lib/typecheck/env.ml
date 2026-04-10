open Parse

module StrMap = Map.Make(String)

type t = Types.t StrMap.t

let find_opt = StrMap.find_opt

let add = StrMap.add

let empty = StrMap.empty

let singleton = StrMap.singleton

let mem = StrMap.mem

let fold = StrMap.fold

module Pretty : sig
  val pp : t Fmt.t
end = struct
  open Types

  let pp_kv ppf k v =
    Fmt.pf ppf "%s |-> %a@,"
    k
    Pretty.pp_t v

  let pp' ppf =
    StrMap.iter (pp_kv ppf)

  let pp =
    Fmt.vbox pp'
end
