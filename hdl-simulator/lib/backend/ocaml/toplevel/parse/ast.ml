open Bits
open Hdlutils

module StrMap = Map.Make (String)

type 'i expr =
  | Const  of bv
  | Record of 'i expr_info StrMap.t

and 'i expr_info = ('i expr, 'i) Info.t

let mk_expr_info (e : 'i expr) (i : 'i) : 'i expr_info =
  Info.make e i

type 'i cmd =
  | Input of 'i expr_info
  | Output
  | Clock
  | Quit

type 'i cmd_info = ('i cmd, 'i) Info.t

let mk_cmd_info (c : 'a cmd) (i : 'i) : 'i cmd_info =
  Info.make c i
