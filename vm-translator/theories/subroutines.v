(*
    subroutines.v

    Subroutines are helper PASM programs.
*)


From Stdlib Require Import List ZArith.
Import ListNotations.

From assembler Require ast reg.
From vm_translator Require ast alias.
Import alias.

Module R := assembler.reg.
Module A := assembler.ast.
Module V := vm_translator.ast.


(* Load 16-bit immediate n into rd using two instructions. *)
Definition li {L:Set} (rd:R.t) (n:Z) : @A.prog L :=
    let lo := Z.land n 255 in
    let hi := Z.land (Z.shiftr n 8) 255 in
    [ A.lli rd lo ;
      A.lui rd hi ].

(* Load 16-bit immediate n into rd using one instruction when possible, else two. *)
Definition li_opt {L:Set} (rd:R.t) (n:Z) : @A.prog L :=
    if andb (Z.leb (-8) n) (Z.leb n 6) then
        [ A.addi rd zero n ]
    else
        li rd n.

(* Move value from register rs to rd. *)
Definition mv {L:Set} (rd rs:R.t) : @A.prog L :=
    [ A.addi rd rs 0%Z ].

(* Pushing to the global stack from a register. *)
Definition push {L:Set} (r:R.t) : @A.prog L :=
    [ A.st r sp 0%Z ;
      A.addi sp sp 1%Z ].

(* Popping from the global stack to a register. *)
Definition pop {L:Set} (r:R.t) : @A.prog L :=
    [ A.addi sp sp (-1)%Z ;
      A.ld r sp 0%Z ].


(* Loading the base-address of a segment into register rd. *)
Definition load_base_addr {L:Set} (rd:R.t) (s:V.segment) : @A.prog L :=
    match s with
    | V.constant  => []
    | V.temp      => li_opt rd 0
    | V.static    => li_opt rd 16
    | V.local     => mv rd lp
    | V.argument  => mv rd ap
    end.


(*
Branch if r1 is equal to r2, but with a larger jump range (due to j).
Labels:
    target  :   branch target
    tl      :   "taken label" -- for implementation
    ntl     :   "not taken label" -- for implementation
 *)

Definition beq {L:Set} (r1 r2:R.t) (target tl ntl:L) : @A.prog L :=
    [ A.beq r1 r2 tl ;
      A.j ntl ;
      A.label tl ;
      A.j target ;
      A.label ntl ].

(* Branch if less than. *)
Definition blt {L:Set} (r1 r2:R.t) (target tl ntl:L) : @A.prog L :=
    [ A.blt r1 r2 tl ;
      A.j ntl ;
      A.label tl ;
      A.j target ;
      A.label ntl ].

(* Branch if greater than. *)
Definition bgt {L:Set} (r1 r2:R.t) (target tl ntl:L) : @A.prog L := blt r2 r1 target tl ntl.


(* Unary operations. *)
Variant unary_op : Set :=
    | neg | not.

(* Translate unary operations. *)
Definition unary {L:Set} (op:unary_op) : @A.prog L :=
    pop w1 ++
    match op with
    | neg => [ A.sub w1 zero w1 ]
    | not => [ A.not w1 w1 ]
    end ++
    push w1.


(* Binary operations. *)
Variant bin_op : Set :=
    | add | sub | and | or.

(* Translate binary operations. *)
Definition bin {L:Set} (op:bin_op) : @A.prog L :=
    pop w1 ++ pop w2 ++
    match op with
    | add   => [ A.add w1 w1 w2 ]
    | sub   => [ A.sub w1 w1 w2 ]
    | and   => [ A.and w1 w1 w2 ]
    (* De Morgan's law. *)
    | or    =>
        [ A.not w1 w1 ;
          A.not w2 w2 ;
          A.and w1 w1 w2 ;
          A.not w1 w1 ]
    end ++
    push w1.


(* Comparison operators (other than eq). *)
Variant comp_op : Set :=
    | lt | gt.

(* Translating comparisons (other than eq). *)
Definition comp {L:Set} (op:comp_op) : @A.prog L :=
    match op with
    | lt    => pop w1 ++ pop w2
    | gt    => pop w2 ++ pop w1
    end ++
    li_opt w3 32768 ++ [
    A.not w4 w3 ;
    A.sub w1 w1 w2 ;
    A.and w1 w1 w3 ;
    A.add w1 w1 w4 ] ++
    push w1.

