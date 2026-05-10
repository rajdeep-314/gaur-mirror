open Hdlutils

type cycle = (string * Loc.t) list

type t =
  | Cycle of cycle list
  | UnboundChip of string

exception CycleError of t

let report_cycles cl =
  raise (CycleError (Cycle cl))

let raise_unbound_chip c =
  raise (CycleError (UnboundChip c))


module Pretty : sig
  val pp : t Fmt.t
end = struct
  let arrow ppf = Fmt.any "->" ppf

  let rec pp ppf err =
    match err with
    | Cycle cl      -> Fmt.pf ppf "@[<v 2>Cycle(s) detected:@,%a@]"
                         Fmt.(vbox (list ~sep:(cut ++ cut) pp_cycle)) cl
    | UnboundChip c -> Fmt.pf ppf "@[<h>Unbounc chip: %s@]" c

  and pp_cycle ppf c =
    Fmt.(vbox (list ~sep:arrow pp_node)) ppf c

  and pp_node ppf (name, loc) =
    Fmt.pf ppf "@[<h>%s (%a)@]"
      name
      Loc.error_fmt loc
end
