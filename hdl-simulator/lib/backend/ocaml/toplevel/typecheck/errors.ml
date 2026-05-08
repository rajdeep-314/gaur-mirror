open Parse

type t =
  | Mismatch  of Types.t * Types.t

exception Error of t * Loc.t

let raise_mismatch loc ta te =
  raise (Error (Mismatch (ta, te), loc))

module Pretty = struct
  open Types.Pretty

  let pp ppf (err, loc) =
    match err with
    | Mismatch (ta, te) ->
      Fmt.pf ppf "@[<v>%s@,The input expression has type %a but an expression was expected of type %a@,@]"
        (Loc.range loc)
        pp_t ta
        pp_t te
end
