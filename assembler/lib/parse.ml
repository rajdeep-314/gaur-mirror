(*
    parser.ml
*)


open Utils.Files

(* Parsing PISA programs. *)

let parse_isa_prog s = Parser_isa.prog Lexer_isa.read_token (Lexing.from_string s)
let parse_isa_prog_from_path path = parse_isa_prog (read_file path)


(* Parsing PASM programs. *)

let parse_asm_prog s = Parser_asm.prog Lexer_asm.read_token (Lexing.from_string s)
let parse_asm_prog_from_path path = parse_asm_prog (read_file path)

