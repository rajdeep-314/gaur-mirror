open Bits

module StrMap = Map.Make(String)

type field = string
type ident = string
type funct = string

type typ = 
  | TBits    of int                     (* bits n *)
  | TRecord  of typ StrMap.t            (* {f1: T, f2: T, ...} *)

type expr =   
  | Constant of bit NonEmpty.t          (* 0b010, 0xfd, 0o71, 32 *)
  | Instance of funct * expr            (* Not 0b1, Not foo.bar, Not in.a *)
  | Splice   of expr * (int * int)      (* foo.bar[0:2], foo.bar[0:0] *)
  | Concat   of expr * expr             (* in.a ++ foo *)
  | Record   of (field * expr) list     (* {a=in.a, b=in.b} *)
  | Access   of ident                   (* foo *)
  | Member   of expr * field            (* foo.bar *)

type typed_expr = {
  e: expr;
  t: typ;
}

type located_expr = {
  expr: expr;
  loc : Lexing.position * Lexing.position
}

type assignment = 
  | Current  of ident * expr            (* Current-state binding *)
  | Next     of ident * expr            (* Next-State binding *)

type chip = { name   : ident
            ; input  : ident * typ
            ; output : ident * typ
            ; states : (typ StrMap.t) option
            ; parts  : assignment list
            }

type program = chip list
