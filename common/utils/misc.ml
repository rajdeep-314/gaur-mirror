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


(* Find a duplicate in a list, if one exists. `mask x` is what equality is
   checked on. *)
let find_duplicate mask xs =
    let seen = Hashtbl.create 16 in
    let rec aux = function
        | [] -> None
        | x :: xs ->
            let masked = mask x in
            if Hashtbl.mem seen masked then Some x
            else
                let () = Hashtbl.add seen masked () in
                aux xs
    in aux xs

(** Compose functions [f] and [g]. *)
let fun_compose f g = fun x -> f (g x)

(** Take the first [n] elements of list [xs]. *)
let rec list_take n xs =
    match xs with
    | []      -> []
    | y :: ys ->
        if n <= 0 then [] else list_take (n - 1) ys

(** Drop the first [n] elements of list [xs]. *)
let rec list_drop n xs =
    match xs with
    | [] -> []
    | _ :: ys ->
        if n <= 0 then [] else list_drop (n - 1) ys

