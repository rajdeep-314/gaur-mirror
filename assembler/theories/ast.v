(*
    ast.v
*)


(**
    {1 Abstract Syntax Module}

    Type for the abstract syntax of Gaur ASM and functions on the same.
*)



From Stdlib Require Import ZArith String List.
Import ListNotations.
Open Scope string_scope.

From assembler_new Require reg.

(** Type for immediate. *)
Definition imm : Set := Z.

(** Type for The assembly's AST.
   l is the type of labels. *)
Inductive t {l:Set} : Set :=
    (** Load  immediate *)
    | li   : reg.t -> imm -> t
    (** Load *)
    | ld    : reg.t -> reg.t -> imm -> t
    (** Store *)
    | st    : reg.t -> reg.t -> imm -> t
    (** Add two registers *)
    | add   : reg.t -> reg.t -> reg.t -> t
    (** Add immediate  to register *)
    | addi  : reg.t -> reg.t -> imm -> t
    (** Subtract two registers *)
    | sub   : reg.t -> reg.t -> reg.t -> t
    (** Bitwise And two registers *)
    | and   : reg.t -> reg.t -> reg.t -> t
    (** Bitwise not two registers *)
    | not   : reg.t -> reg.t -> t
    (** Shift left immediate *)
    | sl    : reg.t -> reg.t -> imm -> t
    (** Shift right immediate *)
    | sr    : reg.t -> reg.t -> imm -> t
    (** Branch equal*)
    | beq   : reg.t -> reg.t -> l -> t
    (** Branch less than *)
    | blt   : reg.t -> reg.t -> l -> t
    (** Jump and link register *)
    | jalr  : reg.t -> reg.t -> imm -> t
    (** Add PC to immediate. *)
    | apc   : reg.t -> imm -> t
    (** Jump to label. *)
    | j     : l -> t
    (** Defining Label. *)
    | label : l -> t
    (** Spin endlessly. *)
    | spin  : t.


(** A program is a list of instructions. *)
Definition prog {l:Set} : Set := list (@t l).


(** Number of instructions in a program. *)
Definition num_insts {l:Set} (p:@prog l) : nat := List.length p.

(** Number of label definitions in a program. *)
Fixpoint num_labels {l:Set} (p:@prog l) : nat :=
    match p with
    | []            => 0
    | label _ :: p' => S (num_labels p')
    | _ :: p'       => num_labels p'
    end.

