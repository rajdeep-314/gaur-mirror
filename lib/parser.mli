(*
    parser.mli
*)


(* parse_prog p parses the textual assembly program p. *)
val parse_prog : string -> string Types.asm_prog

(* prase_prog_from_path p parses the content of the file whose path is p. *)
val parse_prog_from_path : string -> string Types.asm_prog

