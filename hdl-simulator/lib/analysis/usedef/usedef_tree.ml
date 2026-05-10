open Parse.Types

(** A variable is either an [INPUT], [OUTPUT], [CURRENT_STATE], [NEXT_STATE] or
    an [INTERNAL] variable.*)
type var_catg =
  | Input
  | Output
  | CurrentState
  | NextState
  | Internal

type used_flag =  Used | Unused
(** A variable is [Used] if it appears in the RHS of at least one of the
    assignments.
    It is [Unused] otherwise.

    All but [OUTPUT] variables may be used multiple times.
    An [OUTPUT] variable can not be used.

    A [CHIP] is [Used] if it appears in the RHS of at least one of the
    assignments in at least one of the other defined [CHIP]s.
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

type updated_flag = Updated | Unupdated
(** (Only for [STATE] variables)

    A [STATE] is [Updated] if it appears in the LHS of a sequential
    assignment.
    It is [Unupdated] otherwise.

    Once a [STATE] is updated, it can not be updated again.
*)

(** - An [INPUT] variable is [Bound] and [Unused] the moment it enters scope.
    - Update field for an [INPUT] variable is undefined.

    When leaving scope,
    - If an [INPUT] variable is [Unused], an unused-variable warning
      may be issued.
    - If an [INPUT] variable is [Used], no warnings or errors are issued.
*)
(* type var_in_info =
   { used : used_flag
   }

   let mk_in_info =
   { used=Unused
   } *)


(** - An [OUTPUT] variable is [Unbound] and [Unused] the moment it enters scope.
    - It can not be used.
    - Update field for an [OUTPUT] variable is undefined.

    When leaving scope,
    - If an [OUTPUT] variable is [Unbound], an unbound-out-variable error
      is issued.
    - If an [OUTPUT] variable is [Bound], no warnings or errors are issued.
*)
type var_out_info =
  { bound : bound_flag
  }

let mk_out_info =
  { bound=Unbound
  }


(** - An [INTERNAL] variable is [Bound] and [Unused] the moment it enters scope.
    - Update field for an [INTERNAL] variable is undefined.

    When leacing scope,
    - If an [INTERNAL] variable is [Unused], an unused-variable warning
      may be issued.
    - If an [INTERNAL] variable is [Used], no warnings or errors are issued.
*)
(* type var_internal_info =
   { used : used_flag;
   }

   let mk_internal_info =
   { used=Unused;
   } *)


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
type var_state_info =
  { (* used    : used_flag; *)
    updated : updated_flag
  }

let mk_state_info =
  { (* used=Unused; *)
    updated=Unupdated;
  }

module UDGraphV = struct
  type t =
    { name : ident;
      catg : var_catg
    }

  let compare v1 v2 = compare v1.name v2.name
  let hash v = String.hash v.name
  let equal v1 v2 = v1.name = v2.name
  let string_of_vertex v =
    v.name ^
    match v.catg with
    | Input | Output | CurrentState | Internal -> ""
    | NextState -> "'"

end

module UDGraph = Graph.Persistent.Digraph.Concrete(UDGraphV)

let vertices g = UDGraph.fold_vertex (fun v acc -> v :: acc) g []

type ud_graph_info =
  { graph : UDGraph.t;

    chip : ident;

    var_in : ident;

    var_out : ident;
    out_info : var_out_info;

    states_info : var_state_info StrMap.t;

    bound_internals : Set.Make(String).t
  }
