(*
    assemble.mli
*)


(* Assemble a program to an intermediate assembly stage where all labels are
   resolved and there are no pseudo-instructions. *)
val to_inter : string Types.asm_prog -> (Types.isa_prog, string) Result.t

(* Assemble a program fully to a machine code format. *)
val assemble_prog : string Types.asm_prog -> (Types.machine_prog, string) Result.t

