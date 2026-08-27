(*
    bigint.mli
*)


module BI := Big_int

(* The constant 256 of type big_int. *)
val big_int_256 : BI.big_int

(* Convert a big_int to a list of characters (equivalent to a list of bytes). *)
val chars_of_big_int : int -> BI.big_int -> char list

