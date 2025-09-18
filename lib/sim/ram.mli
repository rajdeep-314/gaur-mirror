type addr = int
type half = int
type t

val max_addr : addr

val create : unit -> t
val read : t -> addr -> half
val write : t -> addr -> half -> unit
val read_safe : t -> addr -> half option
val write_safe : t -> addr -> half -> unit option

val print_at_addr : t -> addr -> unit
val print_all_nz : t -> unit
val print_in_range_helper : t -> addr -> addr -> unit
val print_in_range : t -> addr -> addr -> unit

