(*
    parse.ml
*)


let parse_prog s = Parser.prog Lexer.read_tok (Lexing.from_string s)

let parse_from_path path =
    parse_prog (Utils.Files.read_file path)

