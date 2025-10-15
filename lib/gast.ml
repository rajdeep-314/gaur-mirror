(* type bit  = O | I
type bits = bit list

type field = string
type ident = string
type funct = string

type tbits
type trecord

type typ = 
  | TBits    of int                     (* bits n *)
  | TRecord  of (field * typ) list      (* {f1: T, f2: T, ...} *)

type _ expr =   
  | Constant : bits -> tbits expr                    (* 0b010, 0xfd, 0o71, 32 *)
  | Instance : funct * 'a expr -> 'b expr            (* not 0b1, not foo.bar, not in.a *)
  | Splice   : tbits expr * (int * int) -> tbits expr      (* foo.bar[0, 2], foo.bar[0,0] *)
  | Concat   : tbits expr * tbits expr -> tbits expr             (* in.a ++ in.b *)
  | Record   : (field * expr) list -> trecord expr     (* and {a=in.a, b=in.b} *)
  | Access   : ident -> 'a expr                   (* foo *)
  | Member   : trecord expr * field -> 'a expr            (* foo.bar *)

type 'a assignment = 
  | Current  of ident * 'a expr            (* Current-state binding *)
  | Next     of ident * 'a expr            (* Next-State binding *)

type chip = { name   :   ident
            ; input  :   ident * typ
            ; output :   ident * typ
            ; states : ((ident * typ) list) option
            ; parts  : assignment list
            }

type program = chip list *)
