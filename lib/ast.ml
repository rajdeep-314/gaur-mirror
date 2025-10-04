type bit  = O | I
type bits = bit list

type name = string

type t = 
  | TBits   of int              (* bits n *)
  | TRecord of (name * t) list  (* {f1: T, ...} *)

type reference = 
  | Field   of name               (* foo *)
  | Dot     of name * reference   (* foo.bar *)
  | RSplice of reference * (int * int)
  (* | Reference of name * name list option * (int * int) option *)

type expr = 
  | Constant of bits            (* 0b010, 0xfd, 0o71, 32 *)

  | Access   of reference
  | Instance of name * expr     (* not 0b1, not foo.bar, not in.a *)
  | ERecord  of (name * expr) list (* and {a=in.a, b=in.b} *)

(* Using concat or splice on a record will throw a compiletime error message. *)
  | Concat   of expr * expr     (* in.a ++ in.b *)
  | ESplice  of expr * (int * int) (* in.wide_wire[0, 2] *)

type assignment = 
  | Current of reference * expr (* Unprimed variables assignment *)
  | Next    of reference * expr (* Primed variables assignment *)

type chip = {
  name   : name;
  input  : name * t;
  output : name * t;
  states : (name * t) list;
  parts  : assignment list
}

type program = chip list
