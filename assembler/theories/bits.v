(*
    bits.v

    Type for bits and binary numbers as length-indexed bit-vectors, along
    with functions to work with them.
*)


From Stdlib Require Import ZArith String Program List.
Import ListNotations.
Open Scope Z_scope.
Open Scope string_scope.

From assembler Require lemmas.


(* Bits. *)
Inductive t:Set :=
    | b0 | b1.


(* Bit-vectors, indexed by their length. *)
Inductive bv : nat -> Set :=
    | nil   : bv 0
    | cons  : forall n:nat, t -> bv n -> bv (S n).


(* Length of a bit-vector. *)
Definition length (n:nat) (_:bv n) : nat := n.


(* Bit-vector to a list of bits. *)
Fixpoint bv_to_list (n:nat) (v:bv n) : list t :=
    match v with
    | nil           => []
    | cons n' b v'  => b :: bv_to_list n' v'
    end.

(* A bit-vector of size n, of all zeros. *)
Fixpoint zeros (n:nat) : bv n :=
    match n with
    | O     => nil
    | S n'  => cons n' b0 (zeros n')
    end.

(* Concat two bit-vectors of length m and n to a bit-vector of length (m + n). *)
Fixpoint concat (m n:nat) (x:bv m) (y:bv n):bv (m + n):=
    match x with
    | nil           => y
    | cons m' b x'  => cons (m' + n) b (concat m' n x' y)
    end.

(* b1 if z is odd, else b0. *)
Definition bit_parity (z:Z) : t :=
    if Z.odd z then b1 else b0.


(* Converts an integer to a bit-vector of length n.
   NOTE:
   The bit-vector is "reversed", compared to the conventional binary reprsentation.
   For example,
        int_to_bv_n 4 10    ->      ( 0 , 1 , 0 , 1 )
        int_to_bv_n 5 (-10) ->      ( 0 , 1 , 1 , 0 , 1 )
 *)
Fixpoint int_to_bv_n (n:nat) (z:Z) : bv n:=
    match n with
    | O     => nil
    | S n'  => cons n' (bit_parity z) (int_to_bv_n n' (Z.div z 2))
    end.

(* Helper function to convert bit-vector to an unsigned integer *)
Fixpoint bv_to_unsigned (n:nat) (v:bv n) : Z :=
    match v with
    | nil => 0
    | cons n' b v' =>
        let bit_val := match b with b0 => 0 | b1 => 1 end in
        bit_val + 2 * (bv_to_unsigned n' v')
    end.

(* Convert bit-vector to signed integer using two's complement *)
Definition bv_to_int (n:nat) (v:bv n) : Z :=
    match n with
    | O => 0
    | S n' =>
        let unsigned := bv_to_unsigned n v in
        let modulus := 2 ^ (Z.of_nat (S n')) in
        let half_modulus := 2 ^ (Z.of_nat n') in
        if Z.ltb unsigned half_modulus then unsigned else unsigned - modulus
    end.


(* Reverse a bit-vector. *)
Program Fixpoint reverse {n:nat} (v:bv n):(bv n):=
    match v with
    | nil           => nil
    | cons n' b v'  => concat n' 1 (reverse v') (cons 0 b nil)
    end.

Next Obligation.
    induction n' as [| n'' IHn'].
    + reflexivity.
    + apply lemmas.add_1_r.
Defined.


(* For extraction. *)

Inductive bv_:Set :=
    | BV : list t -> bv_.

Definition erase (n:nat) (v:bv n) : bv_ := BV (bv_to_list n v).

