open Hdlutils

type kind

val lexing : kind
val parsing : kind
val dependency : kind
val type_error : kind

type severity

val error : severity
val warning : severity
val bug : severity

type t

val create
  :  kind:kind
  -> severity:severity
  -> code:int
  -> source:Loc.t
  -> summary:string
  -> message:string
  -> t

val pp_detail : t Fmt.t
val pp_short : t Fmt.t
