From Stdlib Require Import ZArith.
From Stdlib Require Import String.
From Stdlib Require Import List.
Import ListNotations.
Open Scope string_scope.

From assembler Require symtable.
From assembler Require reg.
Import reg.
Definition imm :Set := Z.



Inductive t (l : Set) : Set :=
    | lli : reg -> imm -> t l
    | lui : reg -> imm -> t l
    | ld  : reg -> reg -> imm -> t l
    | st  : reg -> reg -> imm -> t l 
    | add : reg -> reg -> reg -> t l
    | addi: reg -> reg -> imm -> t l
    | sub : reg -> reg -> reg -> t l
    | and : reg -> reg -> reg -> t l
    | not : reg -> reg -> t l
    | beq : reg -> reg -> l -> t l
    | blt : reg -> reg -> l -> t l
    | jalr: reg -> reg -> imm -> t l
    | apc : reg -> imm -> t l
    | j   : l -> t l
    (* Pseudos*)
    | label :l -> t l
    | li  : reg -> imm -> t l 
    (* | la  : reg -> l -> t l
    | jal : reg -> imm -> t l (* doubt what do we do with jal rd label*)
    | bgt : reg -> reg -> l -> t l *)
    | spin: t l.


Definition prog (l : Set) := list (t l).

Definition width (l:Set) (inst:t l):nat :=
    match inst with
    | label _ _ => 0
    | spin _ => 1
    | beq _ _ _ _ => 3
    | blt _ _ _ _ => 3
    (* | bgt _ _ _ _ => 3 *)
    | li _ _ _=> 2
    (* | la _ _ _=> 3 *)
    (* | jal _ _ _ => 3 *)
    | _ => 1
    end.


(* Definition symtable (l:Set) : Set := list (l * nat). *)

Fixpoint build_symtable (l : Set) (p : prog l) (count : nat) : symtable.t l :=
  match p with
  | [] => symtable.empty l
  | i :: p' =>
      let count' := count + width l i in
      match i with
      | label _ name => symtable.add l (build_symtable l p' count') name count
      | _ => build_symtable l p' count'
      end
  end.

Definition symtable_of_prog (l:Set) (p:prog l) : symtable.t l :=
    build_symtable l p 0.


Definition inst: t string := label string "s".
Definition sp:prog string:= [inst; label string "l"].

(* Compute (symtable_of_prog string sp). *)

Definition sample_prog : prog string :=
  [
    label string "x";
    li string r1 10%Z;
    li string r2 20%Z;
    label string "y";
    addi string r1 r0 (-1)%Z;
    add string r2 r1 r1;
    label string "meow";
    j string "y"
  ].

(* Compute symtable_of_prog string sample_prog. *)

