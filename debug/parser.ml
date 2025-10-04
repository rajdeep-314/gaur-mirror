open Lexer

let lex_string s =
    let lexbuf = Lexing.from_string s in
    let rec aux () =
        match Lexer.token lexbuf with
        | EOF -> []
        | tok -> tok :: aux ()
    in
    aux ()


let parse_line_from_toks = function
    | HELP :: [] ->
            Ast.Help
    | SET_TRACE :: ON :: [] ->
            Ast.SetTrace true
    | SET_TRACE :: OFF :: [] ->
            Ast.SetTrace false
    | REG :: REGISTER n :: [] ->
            Ast.Reg n
    | REGS :: [] ->
            Ast.Regs
    | MEM :: NUM addr :: NUM data :: [] ->
            Ast.Mem (addr, data)
    | STEP :: [] ->
            Ast.Step
    | STEPS :: NUM n_steps :: [] ->
            Ast.Steps n_steps
    | QUIT :: [] ->
            Ast.Quit
    | _ ->
            failwith "Parsing error: Invalid syntax."


let parse_line s =
    parse_line_from_toks (lex_string s)


