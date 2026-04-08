open Types
open Hdlutils

type 'i expr =
  | Const     of Bit.t NonEmpty.t
  | Record    of 'i expr_info StrMap.t
  | Splice    of 'i expr_info * int * int
  | Concat    of 'i expr_info * 'i expr_info
  | Access    of string
  | Member    of 'i expr_info * string

and 'i expr_info = ('i expr, 'i) Info.t

let mk_expr_info (e : 'i expr) (i : 'i) : 'i expr_info =
  Info.make e i

type 'i rhs =
  | Call      of string * 'i expr_info
  | Expr      of 'i expr_info

type 'i rhs_info = ('i rhs, 'i) Info.t

let mk_rhs_info (r : 'i rhs) (i : 'i) : 'i rhs_info =
  Info.make r i

type 'i assign =
  | Current   of string * 'i rhs_info
  | Next      of string * 'i rhs_info

type 'i assign_info = ('i assign, 'i) Info.t

let mk_assign_info (a : 'i assign) (i : 'i) : 'i assign_info =
  Info.make a i

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

type 'i prog = ('i chip) list
