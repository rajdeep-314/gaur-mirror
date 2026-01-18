(*
    repl.mli
*)


(* Module aliases. *)
module Sast = Syntax.Ast
module Proc = Cpusim.Proc


(* `steps p s tb n` steps through program p on processor state s for n' steps,
   printing traces if tb is true, returning the final processor state.
   `step p s tb` is equivalent to `steps p s tb 1`, and the additional boolean
   returned is true if the PC was out of bounds, and false otherwise. *)
val step : Sast.prog -> Proc.t -> bool -> Proc.t * bool
val steps : Sast.prog -> Proc.t -> bool -> int -> Proc.t

(* `repl p s tb` runs the REPL mode on program p on initial state s with traces
   if tb is true, and without them otherwise. *)
val repl : Sast.prog -> Proc.t -> bool -> unit

