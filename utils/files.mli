(*
    files.mli
*)


val read_file : string -> string
val write_file : string -> string -> unit
val half_words_of_path : string -> int list
val words_of_path : string -> int list

