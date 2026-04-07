{
    open Repl_parser
    open Err.Helper.Lexer
}


let whitespace = [' ' '\t' '\r' '\n']+

let dig = ['0'-'9']
let nat = dig+


rule read_tokens = parse
    | whitespace    { read_tokens lexbuf }
    | "show"        { SHOW }
    | "calls"       { CALLS }
    | "stack"       { STACK }
    | "top"         { TOP }
    | "function"    { FUNCTION }
    | "screen"      { SCREEN }
    | "execute"     { EXECUTE }
    | "steps"       { STEPS }
    | "step"        { STEP }
    | "till"        { TILL }
    | "before"      { BEFORE }
    | "return"      { RETURN }
    | "termination" { TERMINATION }
    | "reset"       { RESET }
    | "exit"        { EXIT }
    | nat           { NUM (int_of_string (Lexing.lexeme lexbuf)) }
    | eof           { EOF }
    | _             { raise unmatched_character }

