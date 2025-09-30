From Stdlib Require Import List.
Import ListNotations.

Definition t (A:Set) : Set := list (A * nat).

Definition empty (A:Set) : t A := [].

(* add : forall A:Set, t A -> A -> nat -> t A. *)
Definition add (A:Set) (tbl:t A) (lbl:A) (n:nat) : t A :=
    (lbl, n) :: tbl.

(* find_opt : forall A:Set, t A -> A -> option nat. *)
Fixpoint find_opt (A:Set) (eq:A -> A -> bool) (tbl:t A) (lbl:A): option nat :=
    match tbl with
    | [] => None
    | (l, n) :: tbl' =>
            if eq l lbl then Some n
            else find_opt A eq tbl' lbl
    end.

