(*
    vm_lexer.mll

    Lexer specification for the VM translator.
*)


{
    open Vm_parser
}

let digit = ['0'-'9']+
let ascii = ['a'-'z' 'A'-'Z' '0'-'9' '-' '_' '.' '$']*
let lf_name = ['a'-'z' 'A'-'Z' '_' '.'] ascii
let whitespace = [' ' '\t' '\r' '\n']+

rule read_token = parse
    | whitespace        { read_token lexbuf }
    | "(*"              { comment 1 lexbuf }
    | "constant"        { CONSTANT }
    | "temp"            { TEMP }
    | "static"          { STATIC }
    | "screen"          { SCREEN }
    | "local"           { LOCAL }
    | "argument"        { ARGUMENT }
    | "push"            { PUSH }
    | "pop"             { POP }
    | "add"             { ADD }
    | "sub"             { SUB }
    | "neg"             { NEG }
    | "eq"              { EQ }
    | "gt"              { GT }
    | "lt"              { LT }
    | "and"             { AND }
    | "or"              { OR }
    | "not"             { NOT }
    | "label"           { LABEL }
    | "goto"            { GOTO }
    | "if-goto"         { IFGOTO }
    | "function"        { FUNCTION }
    | "call"            { CALL }
    | "return"          { RETURN }
    | digit             { NUM (int_of_string (Lexing.lexeme lexbuf)) }
    | lf_name           { ID (Lexing.lexeme lexbuf) }
    | eof               { EOF }
    | _                 { failwith ("Lexing error: Match failed for " ^ Lexing.lexeme lexbuf) }
and comment depth = parse
    | "(*"              { comment (depth +1) lexbuf }
    | "*)"              { if depth = 1 then read_token lexbuf 
                         else comment (depth-1) lexbuf }
    | eof               { failwith "Unterminated comment" }
    | _                 { comment depth lexbuf }
