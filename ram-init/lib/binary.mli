(*
    binary.mli
*)


(** {1 Binary Module}*)

(** Type for binary. *)
type t = Bytes.t

(** [to_string b] converts the binary [b] to string. *)
val to_string : t -> string

(** [print b] prints the binary [b] to the standard output. *)
val print : t -> unit

