open Parse.Asttypes
open Parse.Bits

type expr =
  { expr_desc : expr_desc;
    expr_loc : Parse.Location.loc;
    expr_typ : typ;
    expr_env : Env.t
  }

and expr_desc =
  | Texpr_constant of bit Parse.NonEmpty.t
  (** 0b010, 0xfd, 0o71, 32 *)
  | Texpr_apply of ident * expr
  (** Not 0b1, Not foo.bar, Not in.a *)
  | Texpr_splice of expr * int * int
  (** foo.bar[0:2], foo.bar[0] *)
  | Texpr_concat of expr * expr
  (** foo1 ++ bar2 *)
  | Texpr_record of expr StrMap.t
  (** {a=in.a, b=in.b} *)
  | Texpr_access of ident
  (** foo *)
  | Texpr_member of expr * ident
  (** foo.bar *)

let mk_expr ~loc desc typ env =
  { expr_desc=desc;
    expr_loc=loc;
    expr_typ=typ;
    expr_env=env
  }

type assign =
  { assign_desc : assign_desc;
    assign_loc : Parse.Location.loc;
    assign_env : Env.t
  }

and assign_desc =
  { var : ident;
    expr : expr;
    state : state_desc
  }

let mk_assign ~loc desc env =
  { assign_desc=desc;
    assign_loc=loc;
    assign_env=env
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
    env : Env.t
  }

type program = chip list
