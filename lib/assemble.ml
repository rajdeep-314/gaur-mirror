(* Functions from the extracted Rocq code, just "instantiated" with
   string as the labels' type. *)

let to_inter = Assembler_extr.ast_prog_to_isa_prog_extr String.equal

let assemble_prog = Assembler_extr.assemble_prog_extr String.equal

