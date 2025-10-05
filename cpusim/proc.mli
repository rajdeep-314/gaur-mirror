open Syntax

type pc = int
type t =
    | State of pc * Regfile.t * Ram.t

val create : unit -> t
val get_pc : t -> pc
val get_regfile : t -> Regfile.t
val get_ram : t -> Ram.t

val step_inst : t -> Ast.t -> t
val step_prog : t -> Ast.prog -> t option

val execute_at_pc : t -> Ast.prog -> t
val execute_prog : Ast.prog -> t
val execute_prog_on_ram : Ram.t -> Ast.prog -> t


val step_inst_wt : t -> Ast.t -> t * Trace.t
val step_prog_wt : t -> Ast.prog -> (t * Trace.t) option

val execute_at_pc_wt : t -> Ast.prog -> Trace.t list -> t * (Trace.t list)
val execute_prog_wt : Ast.prog -> t * (Trace.t list)
val execute_prog_on_ram_wt : Ram.t -> Ast.prog -> t * (Trace.t list)

val execute_at_pc_pt : t -> Ast.prog -> t
val execute_prog_pt : Ast.prog -> t
val execute_prog_on_ram_pt : Ram.t -> Ast.prog -> t

