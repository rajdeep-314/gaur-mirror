(*
    ast.v

    Type for the abstract syntax of PASM and some functions on the same.
*)


From Stdlib Require Import ZArith String List.
Import ListNotations.
Open Scope string_scope.

From assembler Require symtable reg.


Definition imm : Set := Z.

(* The assembly's AST.
   l is the type of labels. *)
Inductive t (l:Set) : Set :=
    | lli   : reg.t -> imm -> t l
    | lui   : reg.t -> imm -> t l
    | ld    : reg.t -> reg.t -> imm -> t l
    | st    : reg.t -> reg.t -> imm -> t l 
    | add   : reg.t -> reg.t -> reg.t -> t l
    | addi  : reg.t -> reg.t -> imm -> t l
    | sub   : reg.t -> reg.t -> reg.t -> t l
    | and   : reg.t -> reg.t -> reg.t -> t l
    | not   : reg.t -> reg.t -> t l
    | beq   : reg.t -> reg.t -> l -> t l
    | blt   : reg.t -> reg.t -> l -> t l
    | jalr  : reg.t -> reg.t -> imm -> t l
    | apc   : reg.t -> imm -> t l
    | j     : l -> t l
    (* Pseudo-instructions. *)
    | label : l -> t l
    | spin  : t l.


(* A program is a list of instructions. *)
Definition prog (l:Set) : Set := list (t l).


(* Number of instructions in a program. *)
Definition num_insts (l:Set) (p:prog l) : nat := List.length p.

(* Number of label definitions in a program. *)
Fixpoint num_labels (l:Set) (p:prog l) : nat :=
    match p with
    | [] => 0
    | label _ _ :: p' => S (num_labels l p')
    | _ :: p' => num_labels l p'
    end.

