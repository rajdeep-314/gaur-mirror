open Types
open Hdlutils

type 'i expr =
  | Const     of Bit.t NonEmpty.t
  | Apply     of 'i * 'i expr
  | Splice    of 'i expr * int * int
  | Concat    of 'i expr * 'i expr
  | Record    of 'i expr StrMap.t
  | Access    of string
  | Member    of 'i expr * string

type 'i assign =
  | Current   of 'i * 'i expr
  | Next      of 'i * 'i expr

type ('a, 'f) info =
  { payl : 'a
  ; info : 'f
  }

type ('i, 'f) expr_info   = ('i expr, 'f) info

type ('i, 'f) assign_info = ('i assign, 'f) info

type 'i port =
  { name : 'i
  ; typ  : t
  }

type ('i, 'f) chip =
  { name    : 'i
  ; input   : 'i port
  ; output  : 'i port
  ; states  : (t StrMap.t) option
  ; parts   : ('i, 'f) assign_info list
  }
