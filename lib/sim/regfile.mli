type half = int
type t

val create : unit -> t
val read : t -> Ast.Reg.t -> half
val write : t -> Ast.Reg.t -> half -> unit

val print_reg : t -> Ast.Reg.t -> unit
val print : t -> unit

