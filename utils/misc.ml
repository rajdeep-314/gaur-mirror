(*
    misc.ml

    Miscellaneous utility functions.
*)


let ( let* ) = Option.bind


let hashtbl_of_list l =
    Hashtbl.of_seq (List.to_seq l)

(* First n values of a list l. *)
let rec list_head_opt l n =
    if n = 0 then Some ([], l) else
    match l with
    | []        -> None
    | x :: xs   ->
        let* ys, ys' = list_head_opt xs (n - 1) in Some (x :: ys, ys')

