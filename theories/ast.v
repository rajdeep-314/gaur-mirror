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
    | li    : reg.t -> imm -> t l 
    | spin  : t l.


(* A program is a list of instructions. *)
Definition prog (l:Set) := list (t l).


(* Width of an instruction represents how many ISA instructions it
   expands to.
   This is used to resolve labels. *)
Definition width (l:Set) (inst:t l):nat :=
    match inst with
    | label _ _ => 0
    | spin _ => 1
    | beq _ _ _ _ => 3
    | blt _ _ _ _ => 3
    | li _ _ _=> 2
    | _ => 1
    end.

(* Builds a symbol table out of the labels in a program, with count as the
   base line number. *)
Fixpoint build_symtable (l:Set) (p:prog l) (count:nat) : symtable.t l :=
    match p with
    | [] => symtable.empty l
    | i :: p' =>
            let count' := count + width l i in
            match i with
            | label _ name => symtable.add l (build_symtable l p' count') name count
            | _ => build_symtable l p' count'
            end
    end.

(* Builds a symbol table of a program (with base line number as 0). *)
Definition symtable_of_prog (l:Set) (p:prog l) : symtable.t l :=
    build_symtable l p 0.

