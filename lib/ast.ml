type bit  = O | I
type bits = bit list

type field = string
type ident = string
type funct = string

type typ = 
  | TBits    of int                     (* bits n *)
  | TRecord  of (field * typ) list      (* {f1: T, f2: T, ...} *)

type expr =   
  | Constant of bits                    (* 0b010, 0xfd, 0o71, 32 *)
  | Instance of funct * expr            (* not 0b1, not foo.bar, not in.a *)
  | Splice   of expr * (int * int)      (* foo.bar[0, 2], foo.bar[0,0] *)
  | Concat   of expr * expr             (* in.a ++ in.b *)
  | Record   of (field * expr) list     (* and {a=in.a, b=in.b} *)
  | Access   of ident                   (* foo *)
  | Member   of expr * field            (* foo.bar *)

type typed_expr = {
  e: expr;
  t: typ;
}

type assignment = 
  | Current  of ident * expr            (* Current-state binding *)
  | Next     of ident * expr            (* Next-State binding *)

type chip = { name   :   ident
            ; input  :   ident * typ
            ; output :   ident * typ
            ; states : ((ident * typ) list) option
            ; parts  : assignment list
            }

type program = chip list
