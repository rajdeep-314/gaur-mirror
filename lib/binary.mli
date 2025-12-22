(*
    binary.mli
*)


type t = Bytes.t

(* String representation of binary data and printing function for the same. *)
val to_string : t -> string
val print : t -> unit

