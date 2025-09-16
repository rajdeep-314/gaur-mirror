type addr = int
type half = int
type t

val max_addr : addr

val create : unit -> t
val read : t -> addr -> half
val write : t -> addr -> half -> unit
val read_safe : t -> addr -> half option
val write_safe : t -> addr -> half -> unit option

