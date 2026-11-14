(*
    strings.v
*)

(** {1 Utility functions that deal with strings} *)


From Stdlib Require Import ZArith String Numbers.DecimalString.


(** {2 Some useful strings} *)

(** The "\n" string. *)
Definition newline:string := String (Ascii.ascii_of_nat 10) EmptyString.

(** The "\t" string. *)
Definition tab:string := String (Ascii.ascii_of_nat 9) EmptyString.


(** {2 Some useful string representations} *)

(** [nat_to_string n] is the string representation of natural number [n]
    in its decimal form. *)
Definition nat_to_string (n:nat) : string :=
    DecimalString.NilZero.string_of_uint (Nat.to_uint n).

(** [Z_of_string z] is the string representation of integer [z] in its decimal
    form. *)
Definition Z_to_string (z:Z) : string :=
    DecimalString.NilZero.string_of_int (Z.to_int z).

