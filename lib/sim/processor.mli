type pc = int
type t =
    | State of pc * Regfile.t * Ram.t

val create : unit -> t

val step : t -> Ast.t -> t

