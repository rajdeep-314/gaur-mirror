type addr = int
type half = int

type t =
    | Map of addr * half

type maps = t list

let print_map (Map (addr, data)) =
    Printf.printf "%d\t%d\n" addr data

let print_maps = List.iter print_map

