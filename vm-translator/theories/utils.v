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

(* List of size n, each element being x. *)
Definition list_repeat {A:Set} (x:A) (n:nat) : list A :=
    list_init n (fun _ => x).

