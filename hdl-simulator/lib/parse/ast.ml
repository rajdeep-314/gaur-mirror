open Types
open Hdlutils

type ('a, 'f) info =
  { payl : 'a
  ; info : 'f
  }

type 'i expr =
  | Const     of Bit.t NonEmpty.t
  | Apply     of 'i * 'i expr
  | Splice    of 'i expr * int * int
  | Concat    of 'i expr * 'i expr
  | Record    of 'i expr StrMap.t
  | Access    of string
  | Member    of 'i expr * string

type ('i, 'f) expr_info   = ('i expr, 'f) info

type ('i, 'f) assign =
  | Current   of 'i * ('i, 'f) expr_info
  | Next      of 'i * ('i, 'f) expr_info

type ('i, 'f) assign_info = (('i, 'f) assign, 'f) info

type 'i port =
  { name : 'i
  ; typ  : t
  }

type states = t StrMap.t

type ('i, 'f) chip =
  { name    : 'i
  ; input   : 'i port
  ; output  : 'i port
  ; states  : states option
  ; parts   : ('i, 'f) assign_info list
  }

module Pretty = struct
  let pp_dump_port : string port Fmt.t = fun ppf p ->
    Fmt.pf ppf "%s: %a@"
      p.name
      Pretty.pp_dump_t p.typ
end
