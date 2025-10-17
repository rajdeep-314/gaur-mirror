open Bits

module StrMap = Map.Make(String)

type field = string
type ident = string
type funct = string

type typ = 
  | TBits of int
    (** bits n *)
  | TRecord of typ StrMap.t
    (** {f1: T, f2: T, ...} *)


type expr = 
  { expr_desc : expr_desc;
    expr_loc : Location.loc
  }

and expr_desc =   
  | Constant of bit NonEmpty.t
    (** 0b010, 0xfd, 0o71, 32 *)
  | Instance of funct * expr
    (** Not 0b1, Not foo.bar, Not in.a *)
  | Splice of expr * int * int
    (** foo.bar[0:2], foo.bar[0] *)
  | Concat of expr * expr
    (** foo1 ++ bar2 *)
  | Record of expr StrMap.t
    (** {a=in.a, b=in.b} *)
  | Access of ident
    (** foo *)
  | Member of expr * field
    (** foo.bar *)

let mk_expr ~loc expr = 
  { expr_desc=expr;
    expr_loc=loc
  }


type assign = 
  { assign_desc : assign_desc;
    assign_loc : Location.loc
  }

and assign_desc =
  | Current of ident * expr
    (* Current-state binding *)
  | Next of ident * expr
    (* Next-State binding *)

let mk_assign ~loc assign = 
  { assign_desc=assign;
    assign_loc=loc
  }


type chip = 
  { name : ident;
    input : ident * typ;
    output : ident * typ;
    states : (typ StrMap.t) option;
    parts : assign list;
  }

type program = chip list
