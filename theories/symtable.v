(*
    symtable.v

    Type for a symbol table and some basic functions for the same.
*)


From Stdlib Require Import List.
Import ListNotations.

(* Type for a symbol table mapping A to nat. *)
Definition t (A:Set) : Set := list (A * nat).

(* An empty symbol table. *)
Definition empty (A:Set) : t A := [].

(* Add an entry to a symbol table (oblivious to duplicates). *)
Definition add (A:Set) (tbl:t A) (lbl:A) (n:nat) : t A :=
    (lbl, n) :: tbl.

(* Look for an entry in a symbol table.
   eq is the equality function for A. *)
Fixpoint find_opt (A:Set) (eq:A -> A -> bool) (tbl:t A) (lbl:A): option nat :=
    match tbl with
    | []                => None
    | (l, n) :: tbl'    =>
            if eq l lbl then Some n
            else find_opt A eq tbl' lbl
    end.

