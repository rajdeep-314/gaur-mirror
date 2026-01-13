(*
    utils.mli
*)


(** {1 Utility functions.} *)


(** [get_bits x h l] will return the integer masked between the [h]'th and the [l]'th bits of [x]. *)
val get_bits : int -> int -> int -> int

(** [sign_extend x n] extends the integer [x] to an [n]-bit integer. *)
val sign_extend : int -> int -> int

(** [flatten_results rs] flattens a list of results [rs] to [Ok rs'] if all elements of [rs] are [Ok], otherwise to [Error e], where [Error e] is the first error in [rs]. *)
val flatten_results : ('a,'e) result list -> ('a list, 'e) result

(** [read_program p] reads the binary file with path [p] and returns a list of integers, each integer corresponding to a {i PISA} instruction. *)
val read_program : string -> int list

(** [write_file p x] writes a file to path [p] wtih content [x]. *)
val write_file : string -> string -> unit

