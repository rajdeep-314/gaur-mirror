From Stdlib Require Import ZArith String Program.
Open Scope Z_scope.
Open Scope string_scope.

From assembler Require lemmas.


(* Bits. *)
Inductive t:Set :=
    | b0 | b1.


(* String representation of a bit. *)
Definition bit_to_string (b:bits.t) : string :=
  match b with
  | b0 => "0"
  | b1 => "1"
  end.


(* Bit-vectors, indexed by their length. *)
Inductive bv : nat -> Set :=
    | nil   : bv 0
    | cons  : forall n:nat, t -> bv n -> bv (S n).

(* String representation of a bit-vector. *)
Fixpoint bv_to_string (n:nat) (v:bv n) : string :=
    match v with
    | nil => ""
    | cons n' b v' => bit_to_string b ++ bv_to_string n' v'
    end.


(* A bit-vector of size n, of all zeros. *)
Fixpoint zeros (n:nat) : bv n :=
    match n with
    | O => nil
    | S n' => cons n' b0 (zeros n')
    end.

(* Concat two bit-vectors of length m and n to a bit-vector of length (m + n). *)
Fixpoint concat (m n:nat) (x:bv m) (y:bv n):bv (m + n):=
    match x with
    | nil => y
    | cons m' b x' => cons (m' + n) b (concat m' n x' y)
    end.

(* b1 if z is odd, else b0. *)
Definition bit_parity (z:Z) : t :=
    if Z.odd z then b1 else b0.


(* Converts an integer to a bit-vector of length n.
   NOTE:
   The bit-vector is "reversed", compared to the conventional binary reprsentation.
   For example,
        int_to_bv_n 5 10    ->      ( 0 , 1 , 0 , 1 , 0 )
        int_to_bv_n 5 (-10) ->      ( 0 , 1 , 1 , 0 , 1 )
 *)
Fixpoint int_to_bv_n (n:nat) (z:Z) : bv n:=
    match n with
    | O => nil
    | S n' => cons n' (bit_parity z) (int_to_bv_n n' (Z.div z 2))
    end.


(* Reverse a bit-vector. *)
Program Fixpoint reverse (n:nat) (v:bv n):(bv n):=
    match v with
    | nil => nil
    | cons n' b v' => concat n' 1 (reverse n' v') (cons 0 b nil)
    end.

Next Obligation.
    induction n' as [| n'' IHn'].
    + reflexivity.
    + apply lemmas.add_1_r.
Defined.

