open Parse
open Hdlutils

type t =
  | Mismatch  of Types.t * Types.t

exception Error of Loc.t * t

let raise_mismatch loc ta te =
  raise (Error (loc, Mismatch (ta, te)))

module Pertty = struct
  open Types.Pretty

  let pp ppf err =
    match err with
    | Mismatch (ta, te) ->
      Fmt.pf ppf "The input expression has type %a but an expression was expected of type %a"
        pp_t ta
        pp_t te
end
