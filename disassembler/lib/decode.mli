(*
    decode.mli
*)


(** {1 Decoding instructions} *)

module T := Assembler.Types
module E := Errors
module U := Utils

(**
[int_to_reg n] is register [Ok R<n>] for {m 0 \leq n \leq 15}, and
[Error] {!E.InvalidRegNum} otherwise.
*)
val int_to_reg : int -> (T.reg, E.err) result

(**
Disassembles the integer representation of a PISA instruction. Produces a value
that is either [Ok <PISA instruction>] or [Error <disassembler error>].
*)
val disassemble_inst : int -> (T.isa_inst, E.err) result

(** Disassembles a list of integers to produce a PISA program. *)
val disassemble_prog : int list -> (T.isa_prog, E.err) result

