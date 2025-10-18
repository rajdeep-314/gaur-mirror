open Big_int

type addr = int
type half = big_int

type t =
    | Map of addr * half

type maps = t list

let print_map (Map (addr, data)) =
    Printf.printf "%d\t%s\n" addr (string_of_big_int data)

let print_maps = List.iter print_map

