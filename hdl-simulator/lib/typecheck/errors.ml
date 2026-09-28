open Common.Error
open Parse
open Hdlutils

type t =
  | ArgumentMismatch of Types.t * Types.t
  | AssignmentMismatch of Types.t * Types.t
  | ConcatMismatch of Types.t * Types.t
  | NotAChip of Types.t (* Unreachable exception *)
  | NotARecord of Types.t
  | NotSpliceable of Types.t
  | SpliceOutOfBounds of int * int * int
  | UnboundChip of string
  | UnboundVariable of string
  | UnknownField of string

let code error =
  match error with
  | ArgumentMismatch _ -> 0
  | AssignmentMismatch _ -> 1
  | ConcatMismatch _ -> 2
  | NotAChip _ -> 3
  | NotARecord _ -> 4
  | NotSpliceable _ -> 5
  | SpliceOutOfBounds _ -> 6
  | UnboundChip _ -> 7
  | UnboundVariable _ -> 8
  | UnknownField _ -> 9
;;

let message ppf error =
  let open Types.Pretty in
  match error with
  | SpliceOutOfBounds (lb, ub, l) ->
    Fmt.pf ppf "Splice indices [%d, %d] out of range [0, %d]" lb ub (l - 1)
  | NotSpliceable t -> Fmt.pf ppf "Can not splice an expression of type %a" pp_t t
  | ConcatMismatch (t1, t2) ->
    Fmt.pf ppf "Can not concatenate expressions of types %a and %a" pp_t t1 pp_t t2
  | UnboundVariable id -> Fmt.pf ppf "Unbound variable %s" id
  | UnknownField fld -> Fmt.pf ppf "The record has no field '%s'" fld
  | NotARecord t -> Fmt.pf ppf "The expression has type %a which is not a record" pp_t t
  | ArgumentMismatch (ta, te) ->
    Fmt.pf
      ppf
      "The expression has type %a but an expression was expected of type %a"
      pp_t
      ta
      pp_t
      te
  | NotAChip t -> Fmt.pf ppf "The expression of type %a is not a chip" pp_t t
  | UnboundChip id -> Fmt.pf ppf "Unbound chip %s" id
  | AssignmentMismatch (ta, te) ->
    Fmt.pf
      ppf
      "The variable of type %a can not be assigned an expression of type %a"
      pp_t
      te
      pp_t
      ta
;;

let summary = message

module Pretty = struct
  let pp = message
end

let create source error =
  Reporting.create
    ~kind:Reporting.type_error
    ~code:(code error)
    ~severity:Reporting.error
    ~source
    ~summary:(Fmt.to_to_string summary error)
    ~message:(Fmt.to_to_string message error)
;;

exception TypeError of Reporting.t

let raise_argument_mismatch source actual expected =
  raise (TypeError (create source (ArgumentMismatch (actual, expected))))
;;

let raise_assignment_mismatch source actual expected =
  raise (TypeError (create source (AssignmentMismatch (actual, expected))))
;;

let raise_concat_mismatch source left right =
  raise (TypeError (create source (ConcatMismatch (left, right))))
;;

let raise_not_a_chip source actual = raise (TypeError (create source (NotAChip actual)))

let raise_not_a_record source actual =
  raise (TypeError (create source (NotARecord actual)))
;;

let raise_not_spliceable source actual =
  raise (TypeError (create source (NotSpliceable actual)))
;;

let raise_splice_out_of_bounds source lower upper length =
  raise (TypeError (create source (SpliceOutOfBounds (lower, upper, length))))
;;

let raise_unbound_chip source chip = raise (TypeError (create source (UnboundChip chip)))

let raise_unbound_variable source var =
  raise (TypeError (create source (UnboundVariable var)))
;;

let raise_unknown_field source field =
  raise (TypeError (create source (UnknownField field)))
;;
