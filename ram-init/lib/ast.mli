(*
    ast.mli
*)


open Big_int

type addr = int
type half = big_int

type t = Map of addr * half
type maps = t list

(* Print a mapping. *)
val print_map : t -> unit

(* Print a list of mappings. *)
val print_maps : maps -> unit

