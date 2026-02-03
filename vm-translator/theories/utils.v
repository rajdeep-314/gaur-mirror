(*
    utils.v

    Utility functions.
*)


From Stdlib Require Import List.
Import ListNotations.


(* List of the first n natural numbers (0, 1, ...), in reverse order. *)
Fixpoint nat_list_rev (n:nat) : list nat :=
    match n with
    | 0     => []
    | S n'  => n' :: nat_list_rev n'
    end.

(* List of the first n natural numbers. *)
Definition nat_list (n:nat) : list nat := List.rev (nat_list_rev n).

(* This list of size n: [f 0; f 1; ...; f (n - 1)]. *)
Definition list_init {A:Set} (n:nat) (f:nat -> A) : list A :=
    List.map f (nat_list n).


(* Helper function for list_dup. *)
Fixpoint list_dup_h {A:Set} (eqb:A -> A -> bool) (uniques xs:list A)
    : option A :=
    match xs with
    | []        => None
    | x :: xs'  =>
        match List.find (eqb x) uniques with
        | Some x'   => Some x'
        | None      => list_dup_h eqb (x :: uniques) xs'
        end
    end.

(* true if xs has no duplicates, false otherwise. *)
Definition list_dup {A:Set} (eqb:A -> A -> bool) (xs:list A) : option A :=
    list_dup_h eqb [] xs.

