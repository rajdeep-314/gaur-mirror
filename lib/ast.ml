open Bits

module StrMap = Map.Make(String)

type ident = string

type typ = 
  | TBits of int
  (** bits n *)
  | TRecord of typ StrMap.t
  (** {f1: T, f2: T, ...} *)
  | TChip of typ * typ
  (** And, Or, Mux, Register *)

let rec typ_equal typ1 typ2 = 
  match typ1, typ2 with
  | TBits n, TBits m ->
    n == m
  | TRecord r1, TRecord r2 ->
    StrMap.equal typ_equal r1 r2
  | TChip (tin1, tout1), TChip (tin2, tout2) -> 
    (typ_equal tin1 tin2) && (typ_equal tout1 tout2)
  | _ -> false


type expr = 
  { expr_desc : expr_desc;
    expr_loc : Location.loc
  }

and expr_desc =   
  | Constant of bit NonEmpty.t
  (** 0b010, 0xfd, 0o71, 32 *)
  | Instance of ident * expr
  (** Not 0b1, Not foo.bar, Not in.a *)
  | Splice of expr * int * int
  (** foo.bar[0:2], foo.bar[0] *)
  | Concat of expr * expr
  (** foo1 ++ bar2 *)
  | Record of expr StrMap.t
  (** {a=in.a, b=in.b} *)
  | Access of ident
  (** foo *)
  | Member of expr * ident
  (** foo.bar *)

let mk_expr ~loc expr = 
  { expr_desc=expr;
    expr_loc=loc
  }


type state_desc = 
  | Current
  (* Current-state binding *)
  | Next
  (* Next-State binding *)

type assign = 
  { assign_desc : assign_desc;
    assign_loc : Location.loc
  }

and assign_desc =
  { var : ident;
    expr : expr;
    state : state_desc
  }

let mk_assign ~loc assign = 
  { assign_desc=assign;
    assign_loc=loc
  }

let mk_assign_desc var expr state = 
  { var=var;
    expr=expr;
    state=state
  }


type port =
  { port_name : ident;
    port_type : typ
  }

type chip = 
  { name : ident;
    input : port;
    output : port;
    states : (typ StrMap.t) option;
    parts : assign list;
  }

type program = chip list
