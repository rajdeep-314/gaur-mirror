type addr = int
type half = int

type t =
    | Map of addr * half

type maps = t list

val print_map : t -> unit
val print_maps : maps -> unit

