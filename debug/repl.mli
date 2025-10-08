module Sast = Syntax.Ast
module Proc = Cpusim.Proc


val step : Sast.prog -> Proc.t -> bool -> Proc.t * bool
val steps : Sast.prog -> Proc.t -> bool -> int -> Proc.t
val repl : Sast.prog -> Proc.t -> bool -> unit

