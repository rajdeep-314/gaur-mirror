type pc = int
type t =
    | State of pc * Regfile.t * Ram.t

val create : unit -> t
val get_pc : t -> pc
val get_regfile : t -> Regfile.t
val get_ram : t -> Ram.t

val step : t -> Ast.t -> t
val execute_at_pc : t -> Ast.prog -> t
val execute_prog : Ast.prog -> t

