(*
    ast.v
*)


(**
    {1 Abstract Syntax Module}

    Type for the abstract syntax of PASM and functions on the same.
*)



From Stdlib Require Import ZArith String List.
Import ListNotations.
Open Scope string_scope.

From assembler Require symtable reg.

(** Type for immediate. *)
Definition imm : Set := Z.

(** Type for The assembly's AST.
   l is the type of labels. *)
Inductive t (l:Set) : Set :=
    (** Load lower immediate *)
    | lli   : reg.t -> imm -> t l
    (** Load upper immediate *)
    | lui   : reg.t -> imm -> t l
    (** Load *)
    | ld    : reg.t -> reg.t -> imm -> t l
    (** Store *)
    | st    : reg.t -> reg.t -> imm -> t l
    (** Add two registers *) 
    | add   : reg.t -> reg.t -> reg.t -> t l
    (** Add immediate  to register *)
    | addi  : reg.t -> reg.t -> imm -> t l
    (** Subtract two registers *)
    | sub   : reg.t -> reg.t -> reg.t -> t l
    (** Bitwise And two registers *)
    | and   : reg.t -> reg.t -> reg.t -> t l
    (** Bitwise not two registers *)
    | not   : reg.t -> reg.t -> t l
    (** Branch equal*)
    | beq   : reg.t -> reg.t -> l -> t l
    (** Branch less than *)
    | blt   : reg.t -> reg.t -> l -> t l
    (** Jump and link register *)
    | jalr  : reg.t -> reg.t -> imm -> t l
    (** Add PC to immediate. *)
    | apc   : reg.t -> imm -> t l
    (** Jump to label. *)
    | j     : l -> t l
    (** Defining Label. *)
    | label : l -> t l
    (** Spin endlessly. *)
    | spin  : t l.


(** A program is a list of instructions. *)
Definition prog (l:Set) : Set := list (t l).


(** Number of instructions in a program. *)
Definition num_insts (l:Set) (p:prog l) : nat := List.length p.

(** Number of label definitions in a program. *)
Fixpoint num_labels (l:Set) (p:prog l) : nat :=
    match p with
    | []                => 0
    | label _ _ :: p'   => S (num_labels l p')
    | _ :: p'           => num_labels l p'
    end.
