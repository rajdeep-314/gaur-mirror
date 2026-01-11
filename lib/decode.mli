(*
  decode.mli
*)


module T := Assembler.Types
module E := Err
module U = Utils

val int_to_reg : int -> T.reg
val disassemble_inst : int -> (T.isa_inst, E.err) result
val disassemble_prog : int list -> (T.isa_prog, E.err) result
