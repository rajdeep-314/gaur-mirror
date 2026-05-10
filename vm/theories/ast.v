(*
    ast.v 

    AST of VM programs.
*)


Variant segment:Set :=
    | constant
    | temp
    | static
    | local
    | argument.

Variant inst {L F:Set} : Set :=
    (* Stack operations. *)
    | push   : segment -> nat -> inst
    | pop    : segment -> nat -> inst
    (* Label-related instructions. *)
    | label  : L -> inst
    | goto   : L -> inst
    | ifgoto : L -> inst
    (* Arithmetic operations. *)
    | add | sub | neg
    (* Comparison operators. *)
    | eq | gt | lt
    (* Boolean operators. *)
    | and | or | not
    (* Function-related. *)
    | call  : F -> nat -> inst
    | ret.

Definition insts {L F:Set} : Set := list (@inst L F).

Record function {L F:Set} : Set :=
    Function
    { f_name : F ;
      n_vars : nat ;
      body   : @insts L F }.

Arguments Function {L F}.

Definition prog (L F:Set) : Set := list (@function L F).

