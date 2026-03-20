(*
    misc.ml

    Miscellaneous utility functions.
*)


let ( let* ) = Option.bind


let hashtbl_of_list l =
    Hashtbl.of_seq (List.to_seq l)

(* First n values of a list l and the rest of the list. *)
let rec list_split_opt l n =
    if n = 0 then Some ([], l) else
    match l with
    | []        -> None
    | x :: xs   ->
        let* ys, ys' = list_split_opt xs (n - 1) in Some (x :: ys, ys')

(* Split the list head and the rest of the list. *)
let list_head_split_opt = function
    | []        -> None
    | x :: xs   -> Some (x, xs)


(* Find a duplicate in a list, if one exists. *)
let find_duplicate xs =
    let seen = Hashtbl.create 16 in
    let rec aux = function
        | [] -> None
        | x :: xs ->
            if Hashtbl.mem seen x then Some x
            else
                let () = Hashtbl.add seen x () in
                aux xs
    in aux xs

