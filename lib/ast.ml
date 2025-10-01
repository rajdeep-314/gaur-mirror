type bit  = O | I
type bits = bit list

type name = string

type t = 
  | TBits   of int
  | TRecord of (name * t) list

type reference = 
  | Field of name
  | Dot   of name * reference

type expr = 
  | Constant of bits
  | Instance of name * expr
  | Access   of reference
  | Var      of name
  | ERecord  of (name * expr) list

  (* Using concat or splice on a record will cause it to collapse to a bitvector
     with an associated warning message. *)
  | Concat   of expr * expr
  | Splice   of expr * int * int

type assignment = 
  | Combination of reference * expr
  | Sequential  of reference * expr

type chip = {
  name   : name;
  input  : name * t;
  output : name * t;
  states : (name * t) list;
  parts  : assignment list
}

type program = chip list
