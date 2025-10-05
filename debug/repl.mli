module Sast = Syntax.Ast
module Proc = Cpusim.Proc


val step : Sast.prog -> Proc.t -> bool -> Proc.t option
val steps : Sast.prog -> Proc.t option -> bool -> int -> Proc.t option
val repl : Sast.prog -> Proc.t -> bool -> unit

