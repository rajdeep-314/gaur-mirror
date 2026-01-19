(*
    assemble.mli
*)


(** {1 Assembling programs} *)

(**
[to_inter p] converts a PASM program with string labels into it's intermediate
PISA equivalent, with labels resolved and pseudo-instructions translated.
Returns a value of the [result] type, based on the validity of the input program.
*)
val to_inter : string Types.asm_prog -> (Types.isa_prog, string) Result.t

(**
[assemble_prog p] converts a PASM program with string labels into a PISA
machine program.
Returns a value of the [result] type, like {!to_inter}.
*)
val assemble_prog : string Types.asm_prog -> (Types.machine_prog, string) Result.t

