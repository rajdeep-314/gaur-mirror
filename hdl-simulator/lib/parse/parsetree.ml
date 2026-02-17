open Types
open Hdlutils

type expr =
  { expr_desc : expr_desc;
    expr_loc : Location.loc
  }

and expr_desc =
  | Pexpr_constant of Bit.t NonEmpty.t
  (** 0b010, 0xfd, 0o71, 32 *)
  | Pexpr_apply of ident * expr
  (** Not 0b1, Not foo.bar, Not in.a *)
  | Pexpr_splice of expr * int * int
  (** foo.bar[0:2], foo.bar[0] *)
  | Pexpr_concat of expr * expr
  (** foo1 ++ bar2 *)
  | Pexpr_record of expr StrMap.t
  (** {a=in.a, b=in.b} *)
  | Pexpr_access of ident
  (** foo *)
  | Pexpr_member of expr * ident
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


type chip =
  { name : ident;
    input : port;
    output : port;
    states : (typ StrMap.t) option;
    parts : assign list;
  }

type program = chip list
