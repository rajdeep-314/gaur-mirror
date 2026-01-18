(*
    traces.mli
*)


open Syntax

(* Traces. *)
type t =
    | RegMod of Ram.half * Ast.t * Ast.reg * Ram.half
    | RamMod of Ram.half * Ast.t * Ram.addr * Ram.half
    | NoTrace of Ram.half * Ast.t

(* String representations of traces. *)
val to_string : t -> string

