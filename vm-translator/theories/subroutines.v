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


(* Load 16-bit immediate n into rd. *)
Definition li {L:Set} (rd:R.t) (n:Z) : A.prog L :=
    if andb (Z.leb (-8) n) (Z.leb n 6) then
        [ A.addi L rd zero n ]
    else
        let lo := Z.land n 255 in
        let hi := Z.land (Z.shiftr n 8) 255 in
        [ A.lli L rd lo ;
          A.lui L rd hi ].

(* Move value from register rs to rd. *)
Definition mv {L:Set} (rd rs:R.t) : A.prog L :=
    [ A.addi L rd rs 0%Z ].

(* Pushing to the global stack from a register. *)
Definition push {L:Set} (r:R.t) : A.prog L :=
    [ A.st L r sp 0%Z ;
      A.addi L sp sp 1%Z ].

(* Popping from the global stack to a register. *)
Definition pop {L:Set} (r:R.t) : A.prog L :=
    [ A.addi L sp sp (-1)%Z ;
      A.ld L r sp 0%Z ].


(* Loading the base-address of a segment into register rd. *)
Definition load_base_addr {L:Set} (rd:R.t) (s:V.segment) : A.prog L :=
    match s with
    | V.constant  => []
    | V.temp      => li rd 0
    | V.static    => li rd 16
    | V.local     => mv rd lp
    | V.argument  => mv rd ap
    end.

