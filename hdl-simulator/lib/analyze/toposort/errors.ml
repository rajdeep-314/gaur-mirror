open Hdlutils

type cycle = (string * Loc.t) list

type t =
  | Cycle of cycle list

exception CycleError of t

let report_cycles cl =
  raise (CycleError (Cycle cl))


module Pretty : sig
  val pp : t Fmt.t
end = struct
  let arrow ppf = Fmt.any "->" ppf

  let rec pp ppf err =
    match err with
    | Cycle cl -> Fmt.pf ppf "@[<v 2>Cycle(s) detected:@,%a@]"
                    Fmt.(vbox (list ~sep:(cut ++ cut) pp_cycle)) cl

  and pp_cycle ppf c =
    Fmt.(vbox (list ~sep:arrow pp_node)) ppf c

  and pp_node ppf (name, loc) =
    Fmt.pf ppf "@[<h>%s (%a)@]"
      name
      Loc.error_fmt loc
end
