(*
    lemmas.v
*)


(**
    {1 Lemmas Module}

    This module contains some basic lemmas used across the assembler.
*)


(**
    Lemma add_1_l:
        \[
            \forall n : \mathbb{N},\; n + 1 = \mathrm{S}\,n
        \]

    To prove n+1 = 1+n.
*)
Lemma add_1_r:
    forall n : nat, (n + 1)%nat = S n.
Proof.
    induction n as [| n' IHn'].
    + reflexivity.
    + simpl. rewrite IHn'. reflexivity.
Defined.

