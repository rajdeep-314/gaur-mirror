open Bits
open Hdlutils

module StrMap = Map.Make (String)

type expr =
  | Const  of bv
  | Record of expr_info StrMap.t

and expr_info =
  { expr : expr
  ; loc  : Loc.t
  }

let get_expr e = e.expr

let mk_expr_info e l =
  { expr=e
  ; loc=l
  }

type cmd =
  | Input   of expr_info
  | Output
  | Clock
  | Quit
