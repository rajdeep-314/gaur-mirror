(*
    parse.ml
*)


let parse_line s = Parser.inst Lexer.read_tok (Lexing.from_string s)

let parse_line_opt s =
    try Some (parse_line s) with
    | _ -> None

