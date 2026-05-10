open Hdlutils

type t =
  | Nostate of string
  | Current of string
  | Next    of string

let compare v1 v2 = Stdlib.compare v1 v2

let nostate id = Nostate id
let current id = Current id
let next id = Next id


module Pretty = struct
  let pp_var ppf v =
    match v with
    | Nostate id -> Fmt.pf ppf "@[<h>Stateless %s@]" id
    | Current id -> Fmt.pf ppf "@[<h>Stateful-current %s@]" id
    | Next    id -> Fmt.pf ppf "@[<h>Stateful-next %s@]" id
end
