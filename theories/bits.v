(* Bits and Bit vector  for Int to Bit . used in isa *)
From Stdlib Require Import ZArith.
Open Scope Z_scope.
Inductive t:Set :=
    | b0 | b1.

Inductive bv: nat -> Set :=
| nil : bv 0
| cons: forall n:nat, t -> bv n -> bv (S n).

Fixpoint zeros (n:nat) : bv n :=
    match n with
    | O => nil
    | S n' => cons n' b0 (zeros n')
    end.
    
(* Compute zeros 5.     *)
Fixpoint concat (m n:nat) (x:bv m) (y:bv n):bv (m + n):=
    match x with
    | nil => y
    | cons m' b x' => cons (m' + n) b (concat m' n x' y)
    end.

(* Definition a:bv 5:=zeros 5.
Compute concat 5 5 a a : bv 10. *)

Definition to_bit (z:Z):t :=
    if Z.odd z then b1 else b0.

Fixpoint int_to_bv_n (n:nat) (z:Z) : bv n:=
    match n with
    | O => nil
    | S n' => cons n' (to_bit z) (int_to_bv_n n' (Z.div z 2))
    end.


(* Compute int_to_bv_n 4 8. *)
Definition last_bit {n:nat}(bvec:bv (S n)) : t * bv n :=
match bvec with
| cons n' b bvec' => (b, bvec')
end.

Fixpoint splice (m n:nat) (bvec:bv n): bv m:=
match m, bvec with
| O, _ => nil
| S m', nil => cons m' b0 (splice m' 0 nil)  
| S m', cons n' b bvec' => cons m' b (splice m' n' bvec')
end.


(* Compute int_to_bv_n 4 (-2). *)

    
