(*
    traces.mli
*)


open Syntax

(* Traces. *)
type t =
    | RegMod of Ram.word * Ast.t * Ast.reg * Ram.word
    | RamMod of Ram.word * Ast.t * Ram.addr * Ram.word
    | NoTrace of Ram.word * Ast.t

(* String representations of traces. *)
val to_string : t -> string

