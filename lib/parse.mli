(*
    parser.mli
*)


(* Parsing PISA programs. *)

val parse_isa_prog : string -> Types.isa_prog
val parse_isa_prog_from_path : string -> Types.isa_prog


(* Parsing PASM programs. *)

val parse_asm_prog : string -> string Types.asm_prog
val parse_asm_prog_from_path : string -> string Types.asm_prog

