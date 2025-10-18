(** A variable is either an [INPUT], [OUTPUT] or a [STATE] variable.*)


type used_flag =  Used | Unused
(** A variable is [Used] if it appears in the RHS of at least one of the
    assignments.
    It is [Unused] otherwise.

    All but [OUTPUT] variables may be used multiple times.
    An [OUTPUT] variable can not be used.

    A chip is [Used] if it appears in the RHS of at least one of the assignments
    in at least one of the other defined chips.
    It is [Unused] otherwise.
*)


type bound_flag = Bound | Unbound
(** A variable is [Bound] if it appears in the LHS of an assignment,
    or is an [INPUT] variable.
    It is [Unbound] otherwise.

    Once a variable is bound, it can not be bound again.

    A chip is [Bound] if it has been declared somewhere in the program.
    Otherwise it is [Unbound].

    Once a chip is bound, it can not be bound again.
*)


type updated_flag = Updated | Unupdated
(** (Only for [STATE] variables)

    A state is [Updated] if it appears in the LHS of a sequential
    assignment.
    It is [Unupdated] otherwise.

    Once a state is updated, it can not be updated again.
*)


(** - An [INPUT] variable is [Bound] and [Unused] the moment it enters scope.
    - If an [INPUT] variable is [Unused] when leaving scope,
      a unused-variable warning may be issued.
*)
type in_var_info =
  { mutable used : used_flag
  }


(** - An [OUTPUT] variable is [Unbound] and [Unused] the moment it enters scope.
    - It can not be used.

    - If an [OUTPUT] variable is [Unbound] when leaving scope,
      an unbound-out-variable error is issued.
*)
type out_var_info =
  { mutable bound : bound_flag
  }


(** - A [STATE] variable is [Bound], [Unused] and [Unupdated] the moment it enters scope.

    - If a [STATE] variable is [Unused], a redundancy warning may be issued.
    - If a [STATE] variable is [Used] and [Unupdated], it can be replaced by
      a constant, and a constant-replacement warning may be issued.
    - If a [STATE] variable is [Used] and [Updated], no warnings or errors are issued.
*)
type state_var_info =
  { mutable used : used_flag;
    mutable updated : updated_flag
  }


(** - A [CHIP] is [Bound] the moment it is declared by the program.
    - If a [CHIP] is [Unused] after typechecking,
      a redundancy warning may be issued.
*)
type chip_info =
  { mutable used : used_flag;
  }

(*
type flags =
  { mutable used : used_flag;
    mutable bound : bound_flag;
    mutable updated : updated_flag;
  } *)


(** Flags for three kinds of variables, [INPUT], [OUTPUT] and [STATE],
    and for [CHIP]s.
*)
(* type symbol_info =
   | FIn of in_var_info
   | FOut of out_var_info
   | FState of state_var_info
   | FChip of chip_info *)

type symbol_category = 
  | Input
  | Output
  | State
  | Chip

type symbol_info =
  { typ : Ast.typ;
    category : symbol_category;
    mutable used : used_flag;
    mutable bound : bound_flag;
    mutable updated : updated_flag;
  }
