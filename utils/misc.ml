(*
    misc.ml

    Miscellaneous utility functions.
*)


let hashtbl_of_list l =
    Hashtbl.of_seq (List.to_seq l)

