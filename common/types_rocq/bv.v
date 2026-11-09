(*
    bv.v
*)


From Stdlib Require Vector.
From Stdlib Require Import String ZArith DecimalString ssrfun.
Import Vector.VectorNotations.

Open Scope string_scope.
Open Scope Z_scope.


(** {1 Bit-vector indexed with length} *)

(** [t n] denotes a boolean/bit-vector of length [n]. *)
Definition t:nat -> Type := Vector.t bool.

(** [to_int_unsigned v] interprets [v] as an unsigned MSB-first integer and
    returns the equivalent integer. *)
Definition to_int_unsigned {n:nat} (v:Vector.t bool n) : Z :=
  Vector.fold_left (fun a b => 2*a + Z.b2z b) 0 v.

(** [to_int_signed v] interprets [v] as a signed MSB-first integer and returns
    the equivalent integer. *)
Definition to_int_signed {n:nat} (v:Vector.t bool n) : Z :=
  let h := 2 ^ (Z.of_nat n - 1) in
  (to_int_unsigned v + h) mod (2*h) - h.

(** [of_Z z n] returns the [n]-length bit-vector in MSB corresponding to the
    integer [z]. Overflows and underflows are ignored. *)
Fixpoint of_Z (z:Z) (n:nat) : t n :=
    match n with
    | O    => Vector.nil bool
    | S n' => (Z.testbit z (Z.of_nat  n')) :: of_Z z n'
    end.


(** Module [Printers] contains string representation(s) of bit-vectors. *)
Module Printers.

    (** [decimal_unsigned v] is the decimal representation of [v] being
        interpreted as an [n]-bit MSB-first unsigned integer. *)
    Definition decimal_unsigned {n:nat} : t n -> string :=
        NilEmpty.string_of_int \o Z.to_int \o to_int_unsigned.

    (** [decimal_signed v] is the decimal representation of [v] being
        interpreted as an [n]-bit MSB-first signed integer. *)
    Definition decimal_signed {n:nat} : t n -> string :=
        NilEmpty.string_of_int \o Z.to_int \o to_int_signed.

End Printers.

