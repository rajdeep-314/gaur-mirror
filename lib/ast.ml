type bit  = O | I
type bits = bit list

type field = string
type value = string
type funct = string
type ident = string

type t = 
  (* bits n *)
  | TBits of int
  (* {f1: T, ...} *)
  | TRecord of (string * t) list

type pattern = 
  (* foo *)
  | Variable of value
  (* foo.bar *)
  | PAccess of pattern * field
  (* foo.bar[1..6] *)
  | PSplice of pattern * (int * int)

type expr = 
  (* 0b010, 0xfd, 0o71, 32 *)
  | Constant of bits

  (* not 0b1, not foo.bar, not in.a *)
  | Instance of funct * expr

  (* in.wide_wire[0, 2] *)
  | ESplice  of expr * (int * int)
  (* in.a ++ in.b *)
  | Concat of expr * expr
  
  (* and {a=in.a, b=in.b} *)
  | ERecord  of (field * expr) list
  (* foo.bar *)
  | EAccess of expr * field

type assignment = 
  (* Unprimed variables assignment *)
  | Current of pattern * expr
  (* Primed variables assignment *)
  | Next    of pattern * expr

type chip = {
  name   : ident;
  input  : ident * t;
  output : ident * t;
  states : (ident * t) list;
  parts  : assignment list
}

type program = chip list
