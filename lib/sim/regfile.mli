type half
type t

val create : unit -> t
val read : t -> int -> half
val write : t -> int -> half -> unit

