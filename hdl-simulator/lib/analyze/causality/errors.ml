open Common.Error
open Hdlutils

(** Causality analysis error details *)
type error_detail =
  | InstantaneousCycle of
      { chip_name : string
      ; cycle : Path.t list
      ; explanations : string list
      }
  | UnboundChip of string

type t =
  { loc : Loc.t
  ; detail : error_detail
  }

(** Stable error codes for causality diagnostics *)
let code detail =
  match detail with
  | InstantaneousCycle _ -> 0
  | UnboundChip _ -> 1
;;

(** Pretty-prints error message with cycle trajectory and hop explanations *)
let message ppf detail =
  match detail with
  | InstantaneousCycle { chip_name; cycle; explanations } ->
    let cycle_str = String.concat " -> " (List.map Path.to_string cycle) in
    Fmt.pf ppf "@[<v 0>Instantaneous cycle detected in chip '%s':@,  %s@," chip_name cycle_str;
    if explanations <> [] then begin
      Fmt.pf ppf "Cycle path breakdown:@,";
      List.iter (fun expl -> Fmt.pf ppf "  * %s@," expl) explanations
    end;
    Fmt.pf ppf "@]"
  | UnboundChip id ->
    Fmt.pf ppf "Chip '%s' was not found in the causality summary environment" id
;;

let summary detail = Fmt.to_to_string message detail

(** Constructs a Diagnostics.t record for causality errors *)
let create loc detail =
  Diagnostics.create
    ~kind:Diagnostics.dependency
    ~code:(code detail)
    ~severity:Diagnostics.error
    ~source:loc
    ~summary:(summary detail)
    ~message:(Fmt.to_to_string message detail)
;;

exception CausalityError of Diagnostics.t

(** Helper to raise a combinational cycle diagnostic error *)
let raise_cycle loc chip_name cycle explanations =
  raise (CausalityError (create loc (InstantaneousCycle { chip_name; cycle; explanations })))
;;

(** Helper to raise an unbound subchip diagnostic error *)
let raise_unbound_chip loc id =
  raise (CausalityError (create loc (UnboundChip id)))
;;

