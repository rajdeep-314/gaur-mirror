(*
    parser.ml

    Parser for the textual RAM file.
*)


open Lexer
open Big_int

let lex_string s =
    let lexbuf = Lexing.from_string s in
    let rec aux () =
        match Lexer.token lexbuf with
        | EOF -> []
        | tok -> tok :: aux ()
    in
    aux ()

let parse_line_from_toks = function
    | NUM addr :: COLON :: NUM data :: ts->
            (Ast.Map (int_of_big_int addr, data), ts)
    | _ ->
            failwith "Parsing error: Invalid syntax."

let rec parse_lines_from_toks ts =
    match ts with
    | [] -> []
    | [EOF] -> []
    | NEWLINE :: ts' -> parse_lines_from_toks ts'
    | _ ->
            let (m, ts') = parse_line_from_toks ts in
            m :: parse_lines_from_toks ts'

let parse_maps s =
    let ts = lex_string s in
    parse_lines_from_toks ts

