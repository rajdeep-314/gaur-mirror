open Syntax

type t =
    | RegMod of Ram.half * Ast.t * Ast.Reg.t * Ram.half
    | RamMod of Ram.half * Ast.t * Ram.addr * Ram.half
    | NoTrace of Ram.half * Ast.t

val to_string : t -> string

