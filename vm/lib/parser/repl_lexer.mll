{
    open Repl_parser
}


let whitespace = [' ' '\t' '\r' '\n']+

let dig = ['0'-'9']
let nat = dig+


rule read_tokens = parse
    | whitespace    { read_tokens lexbuf }
    | "show"        { SHOW }
    | "calls"       { CALLS }
    | "stack"       { STACK }
    | "function"    { FUNCTION }
    | "execute"     { EXECUTE }
    | "steps"       { STEPS }
    | "step"        { STEP }
    | "till"        { TILL }
    | "before"      { BEFORE }
    | "return"      { RETURN }
    | "termination" { TERMINATION }
    | "exit"        { EXIT }
    | nat           { NUM (int_of_string (Lexing.lexeme lexbuf)) }
    | eof           { failwith "Lexing error: Can't match." }

