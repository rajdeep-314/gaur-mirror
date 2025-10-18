open Big_int

type addr = int
type half = big_int

type t =
    | Map of addr * half

type maps = t list

val print_map : t -> unit
val print_maps : maps -> unit

