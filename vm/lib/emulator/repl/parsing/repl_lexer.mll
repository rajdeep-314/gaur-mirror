{
    open Repl_parser
    open Err.Helper.Lexer
}


let whitespace = [' ' '\t' '\r' '\n']+
let iden = ['a'-'z' 'A'-'Z' '0'-'9' '-' '_' '.' '$']*

let dig = ['0'-'9']
let nat = dig+

let char = '\'' [' '-'~'] '\''


rule read_tokens = parse
    | whitespace    { read_tokens lexbuf }
    | "help"        { HELP }
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
    | "next"        { NEXT }
    | "breakpoint"  { BREAKPOINT }
    | "temp"        { TEMP }
    | "static"      { STATIC }
    | "local"       { LOCAL }
    | "argument"    { ARGUMENT }
    | "frames"      { FRAMES }
    | "frame"       { FRAME }
    | "state"       { STATE }
    | "segment"     { SEGMENT }
    | "keyboard"    { KEYBOARD }
    | "set"         { SET }
    | "unset"       { UNSET }
    | nat           { NUM (int_of_string (Lexing.lexeme lexbuf)) }
    | char          { CHAR (Lexing.lexeme lexbuf).[1] }
    | iden          { ID (Lexing.lexeme lexbuf) }
    | eof           { EOF }
    | _             { raise unmatched_character }

