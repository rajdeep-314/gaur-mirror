(*
    utils.v
*)


From Stdlib Require Import ZArith String Numbers.DecimalString.


(** [result_map f r] returns [Ok (f v)] if [r] is [Ok v], and if [r] is
    [Error e], then [Error e] is returned. *)
Definition result_map {A B E:Type} (f:A -> B) (r:result A E) : result B E :=
    match r with
    | Ok v    => Ok (f v)
    | Error e => Error e
    end.

(** [po2_signed_range nbits] returns [(-(2^m), 2^m - 1)] where [m] is
    [n - 1]. "po2" stands for "power of 2".
    NOTE: It is expected that [nbits > 0]. *)
Definition po2_signed_range (nbits:nat) : Z * Z :=
    let n := Z.of_nat (nbits - 1) in
    let magnitude := (2^n)%Z in
    (-magnitude, magnitude - 1)%Z.

(** [nat_to_string n] is the string representation of natural number [n]
    in its decimal form. *)
Definition nat_to_string (n:nat) : string :=
    DecimalString.NilZero.string_of_uint (Nat.to_uint n).

(** [Z_of_string z] is the string representation of integer [z] in its decimal
    form. *)
Definition Z_to_string (z:Z) : string :=
    DecimalString.NilZero.string_of_int (Z.to_int z).

