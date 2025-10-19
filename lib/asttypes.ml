type used_flag =  Used | Unused
(** A variable is [Used] if it appears in the RHS of at least one of the
    assignments.
    It is [Unused] otherwise.

    All but [OUTPUT] variables may be used multiple times.
    An [OUTPUT] variable can not be used.

    A [CHIP] is [Used] if it appears in the RHS of at least one of the assignments
    in at least one of the other defined [CHIP]s.
    It is [Unused] otherwise.
*)


type bound_flag = Bound | Unbound
(** A variable is [Bound] if it appears in the LHS of an assignment,
    or is an [INPUT] variable.
    It is [Unbound] otherwise.

    Once a variable is bound, it can not be bound again.

    A [CHIP] is [Bound] if it has been declared somewhere in the program.
    Otherwise it is [Unbound].

    Once a [CHIP] is bound, it can not be bound again.
*)


type updated_flag = Updated | Unupdated | Update_undef
(** (Only for [STATE] variables)

    A [STATE] is [Updated] if it appears in the LHS of a sequential
    assignment.
    It is [Unupdated] otherwise.

    Once a [STATE] is updated, it can not be updated again.
*)


(** A variable is either an [INPUT], [OUTPUT], [STATE] or [INTERNAL] variable.*)
type symbol_category =
  | Input
  | Output
  | State
  | Internal
  | Chip


type symbol_info =
  { typ : Ast.typ;
    category : symbol_category;
    mutable used : used_flag;
    mutable bound : bound_flag;
    mutable updated : updated_flag;
  }


(** - An [INPUT] variable is [Bound] and [Unused] the moment it enters scope.
    - Update field for an [INPUT] variable is undefined.

    When leaving scope,
    - If an [INPUT] variable is [Unused], an unused-variable warning
      may be issued.
    - If an [INPUT] variable is [Used], no warnings or errors are issued.
*)
let mk_in_info typ_in =
  { typ=typ_in;
    category=Input;
    used=Unused;
    bound=Bound;
    updated=Update_undef
  }


(** - An [OUTPUT] variable is [Unbound] and [Unused] the moment it enters scope.
    - It can not be used.
    - Update field for an [OUTPUT] variable is undefined.

    Wen leaving scope,
    - If an [OUTPUT] variable is [Unbound], an unbound-out-variable error
      is issued.
    - If an [OUTPUT] variable is [Bound], no warnings or errors are issued.
*)
let mk_out_info typ_out =
  { typ=typ_out;
    category=Output;
    used=Unused;
    bound=Unbound;
    updated=Update_undef
  }


(** - A [STATE] variable is [Bound], [Unused] and [Unupdated]
    the moment it enters scope.

    When leaving scope,
    - If a [STATE] variable is [Unused], an unused-variable warning
      may be issued.
    - If a [STATE] variable is [Used] and [Unupdated], it can be replaced by
      a constant, and a constant-replacement warning may be issued.
    - If a [STATE] variable is [Used] and [Updated], no warnings or errors
      are issued.
*)
let mk_state_info typ_st =
  { typ=typ_st;
    category=State;
    used=Unused;
    bound=Bound;
    updated=Unupdated
  }


(** - An [INTERNAL] variable is [Bound] and [Unused] the moment it enters scope.
    - Update field for an [INTERNAL] variable is undefined.

    When leacing scope,
    - If an [INTERNAL] variable is [Unused], an unused-variable warning
      may be issued.
    - If an [INTERNAL] variable is [Used], no warnings or errors are issued.
*)
let mk_internal_info typ_int =
  { typ=typ_int;
    category=Internal;
    used=Unused;
    bound=Bound;
    updated=Update_undef;
  }


(** - A [CHIP] is [Bound] the moment it is declared by the program.

    After typechecking,
    - If a [CHIP] is [Unused], an unused-chip warning may be issued.
    - If a [CHIP] is [Used], no warnings or errors are issued.
*)
let mk_chip_info typ_ch =
  { typ=typ_ch;
    category=Chip;
    used=Unused;
    bound=Bound;
    updated=Update_undef
  }
