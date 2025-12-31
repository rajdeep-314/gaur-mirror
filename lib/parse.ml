(*
    parser.ml
*)


open Utils

let parse_prog s = Parser.prog Lexer.read_token (Lexing.from_string s)

let parse_prog_from_path path = parse_prog (read_file path)

