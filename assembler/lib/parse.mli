(*
    parser.mli
*)


(** {1 Parsers} *)


(** {2 Parsing PISA programs} *)

(**
[parse_isa_prog str] parses a PISA program from a string [str].
*)
val parse_isa_prog : string -> Types.isa_prog

(**
[parse_isa_prog_from_path p] parses a PISA program from a file
at the given path [p].
 *)
val parse_isa_prog_from_path : string -> Types.isa_prog


(** {2 Parsing PASM programs} *)

(**
[parse_asm_prog str] parses a PASM program from a string [str].
*)
val parse_asm_prog : string -> string Types.asm_prog

(**
[parse_asm_prog_from_path p] parses a PASM program from a file
at the given path [p].
 *)
val parse_asm_prog_from_path : string -> string Types.asm_prog

