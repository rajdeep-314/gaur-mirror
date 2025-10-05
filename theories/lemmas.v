(* n + 1 is the same as 1 + n for all natural numbers n. *)
Lemma add_1_r:
    forall n : nat, (n + 1)%nat = S n.
Proof.
    induction n as [| n' IHn'].
    + reflexivity.
    + simpl. rewrite IHn'. reflexivity.
Defined.

