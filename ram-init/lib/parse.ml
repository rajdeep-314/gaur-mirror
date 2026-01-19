
(*
    parser.ml
*)


let parse_maps s = Parser.prog Lexer.read_toks (Lexing.from_string s)

