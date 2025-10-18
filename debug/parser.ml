open Lexer

let lex_string s =
    let lexbuf = Lexing.from_string s in
    let rec aux () =
        match Lexer.token lexbuf with
        | EOF -> []
        | tok -> tok :: aux ()
    in
    aux ()


let parse_line_from_toks_opt = function
    | HELP :: [] ->
            Some Ast.Help
    | SET_TRACE :: ON :: [] ->
            Some (Ast.SetTrace true)
    | SET_TRACE :: OFF :: [] ->
            Some (Ast.SetTrace false)
    | GET_TRACE :: [] ->
            Some Ast.GetTrace
    | REG :: REGISTER n :: [] ->
            Some (Ast.Reg n)
    | REGS :: [] ->
            Some Ast.Regs
    | PRINT_INST :: [] ->
            Some Ast.Print_inst
    | PRINT_PROG :: [] ->
            Some Ast.Print_prog
    | MEM :: NUM addr :: NUM data :: [] ->
            Some (Ast.Mem (addr, data))
    | STEP :: [] ->
            Some Ast.Step
    | STEPS :: NUM n_steps :: [] ->
            Some (Ast.Steps n_steps)
    | QUIT :: [] ->
            Some Ast.Quit
    | INVALID :: _ ->
            None
    | EOF :: [] ->
            Some Ast.Quit
    | _ ->
            None


let parse_line_from_toks ts =
    match parse_line_from_toks_opt ts with
    | Some cmd -> cmd
    | None -> failwith "Parsing error: Syntax error."


let parse_line_opt s =
    parse_line_from_toks_opt (lex_string s)

let parse_line s =
    parse_line_from_toks (lex_string s)


