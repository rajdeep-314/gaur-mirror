(*
    repl.ml

    Parser for REPL commands.
*)


let parse_cmd s = Repl_parser.cmd Repl_lexer.read_tokens (Lexing.from_string s)

