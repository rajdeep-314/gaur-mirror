(** Printing error diagnostics *)

open Hdlutils

(** Phase in which error was encountered. *)
type kind

val lexing : kind
val parsing : kind
val dependency : kind
val type_error : kind

(** Severity of error, [error] or [bug]. Warnings are not supported. *)
type severity

val error : severity
val bug : severity

(** Type of error diagnostics. *)
type t

(** [create ~kind:k ~severity:s ~code:e ~source:l ~summary:s ~message:m]
    creates an error diagnostic of kind [k], severity [s], with error code
    [ec], source of error [l], summary message [s] and detailed message [m]. *)
val create
  :  kind:kind
  -> severity:severity
  -> code:int
  -> source:Loc.t
  -> summary:string
  -> message:string
  -> t

(** Print error diagnostics in detail. *)
val pp_detail : t Fmt.t

(** Print error diagnostics in short. *)
val pp_short : t Fmt.t
