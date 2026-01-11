(*
    utils.mli
*)

val get_bits : int -> int -> int -> int
val sign_extend : int -> int -> int
val flatten_results : ('a,'e) result list -> ('a list, 'e) result
val read_program : string -> int list