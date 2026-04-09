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


module Map = struct
  let rec expr f e =
    match e with
    | Const  bv           -> Const bv
    | Record r            -> Record (StrMap.map (expr_info f) r)
    | Splice (e', lb, ub) -> Splice (expr_info f e', lb, ub)
    | Concat (e1, e2)     -> Concat (expr_info f e1, expr_info f e2)
    | Access id           -> Access id
    | Member (e', fld)    -> Member (expr_info f e', fld)

  and expr_info f ei =
    Info.map (expr f) f ei

  let rhs f r =
    match r with
    | Call (id, ei) -> Call (id, expr_info f ei)
    | Expr ei       -> Expr (expr_info f ei)

  let rhs_info f ri =
    Info.map (rhs f) f ri

  let assign f a =
    match a with
    | Current (id, r) -> Current (id, rhs_info f r)
    | Next    (id, r) -> Next    (id, rhs_info f r)

  let assign_info f ai =
    Info.map (assign f) f ai

  let chip f c =
    { name=c.name
    ; input=c.input
    ; output=c.output
    ; states=c.states
    ; parts=List.map (assign_info f) c.parts
    }

  let prog f p =
    List.map (chip f) p
end
