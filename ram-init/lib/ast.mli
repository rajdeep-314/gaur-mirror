(*
    ast.mli
*)


(** {1 Abstract Syntax Types} *)
open Big_int

(** address *)
type addr = int

(** halfword *)
type half = big_int

(** A mapping from an address to a halfword. *)
type t = Map of addr * half

(** List of maps *)
type maps = t list

(** [print_map m] prints the content of the map [m].*)
val print_map : t -> unit

(** [print_maps l] print the contents of all the maps in the list [l]*)
val print_maps : maps -> unit

