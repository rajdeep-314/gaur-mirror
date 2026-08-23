(** Driver code for (lexing and) parsing ASM programs. *)

open Utils.Files

(* Parsing ASM programs. *)

let parse_asm_prog s =
    Parser_asm.prog Lexer_asm.read_token (Lexing.from_string s)
let parse_asm_prog_from_path path = parse_asm_prog (read_file path)

