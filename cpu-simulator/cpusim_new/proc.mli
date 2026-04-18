(*
    proc.mli
*)


module Syntax := Syntax_new
open Syntax

(* Type for program counter. *)
type pc = int
(* Type for the processor's state - PC, register-file and RAM. *)
type t = State of pc * Regfile.t * Ram.t

(* Create an inital state with PC 0 and all-zero register-file
   and RAM. *)
val create : unit -> t

(* Get the contents of a state. *)
val get_pc : t -> pc
val get_regfile : t -> Regfile.t
val get_ram : t -> Ram.t

(* Step through an instruction or through a program on a given
   state and obtain the resultant state. *)
val step_inst : t -> Ast.t -> t
val step_prog : t -> Ast.prog -> t option

(* execute_at_pc ~max_steps:m s p s' means that executing p on state s from the
   PC of s results in state s' with no more than m steps. *)
val execute_at_pc : ?max_steps : int option -> t -> Ast.prog -> t

(* execute_prog ~max_steps:m p executes on the default initial state with no
   more than m steps. *)
val execute_prog : ?max_steps : int option -> Ast.prog -> t

(* execute_prog_on_ram ~max_steps:m r p executes p on the initial RAM r with no
   more than m steps. *)
val execute_prog_on_ram : ?max_steps : int option -> Ram.t -> Ast.prog -> t

(* step_inst_wt s i executes i on state s and returns the resultant state and
   the generated trace. *)
val step_inst_wt : t -> Ast.t -> t * Trace.t
(* The program version of step_inst_wt. *)
val step_prog_wt : t -> Ast.prog -> (t * Trace.t) option

(* Execute a program with the traces. *)
val execute_at_pc_wt:
    ?max_steps : int option -> t -> Ast.prog -> Trace.t list -> t * (Trace.t list)
val execute_prog_wt : ?max_steps : int option -> Ast.prog -> t * (Trace.t list)
val execute_prog_on_ram_wt:
    ?max_steps : int option -> Ram.t -> Ast.prog -> t * (Trace.t list)

(* Execute a program while printing the traces. *)
val execute_at_pc_pt : ?max_steps : int option -> t -> Ast.prog -> t
val execute_prog_pt : ?max_steps : int option -> Ast.prog -> t
val execute_prog_on_ram_pt : ?max_steps : int option -> Ram.t -> Ast.prog -> t

