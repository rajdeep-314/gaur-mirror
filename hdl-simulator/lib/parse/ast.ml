open Types
open Hdlutils

type ('d, 'i) info =
  { data : 'd
  ; info : 'i
  }

type 'i expr =
  | Const     of Bit.t list
  | Splice    of 'i expr_info * int * int
  | Concat    of 'i expr_info * 'i expr_info
  | Access    of string
  | Member    of 'i expr_info * string

and 'i expr_info = ('i expr, 'i) info

type 'i args = 'i expr_info StrMap.t

type 'i call =
  | CallE     of string * 'i expr_info
  | CallR     of string * 'i args

type 'i call_info = ('i call, 'i) info

type 'i rhs =
  | Call      of 'i call_info
  | Expr      of 'i expr_info

type 'i rhs_info = ('i rhs, 'i) info

type 'i assign =
  | Current   of string * 'i rhs_info
  | Next      of string * 'i rhs_info

type 'i assign_info = ('i assign, 'i) info

type port =
  { name : string
  ; typ  : t
  }

type states = t StrMap.t

type 'i chip =
  { name    : string
  ; input   : port
  ; output  : port
  ; states  : states option
  ; parts   : 'i assign_info list
  }
