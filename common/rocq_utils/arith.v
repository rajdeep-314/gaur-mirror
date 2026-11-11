(*
    arith.v
*)

(** {1 Arithmetic-related utility functions} *)


From Stdlib Require Import ZArith.


(** [po2_signed_range nbits] returns [(-(2^m), 2^m - 1)] where [m] is
    [nbits - 1]. "po2" stands for "power of 2".
    NOTE: It is expected that [nbits > 0]. *)
Definition po2_signed_range (nbits:nat) : Z * Z :=
    let n := Z.of_nat (nbits - 1) in
    let magnitude := (2^n)%Z in
    (-magnitude, magnitude - 1)%Z.

(** [po2_unsigned_range nbits] returns [(0, 2^nbits - 1)]. *)
Definition po2_unsigned_range (nbits:nat) : Z * Z :=
    let n := Z.of_nat nbits in
    (0, (2^n) - 1)%Z.

