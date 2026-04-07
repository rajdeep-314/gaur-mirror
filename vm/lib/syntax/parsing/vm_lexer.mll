(*
    vm_lexer.mll

    Lexer specification for the VM translator.
*)


{
    open Vm_parser
    open Err.Helper.Lexer
}

let digit = ['0'-'9']+
let iden = ['a'-'z' 'A'-'Z' '0'-'9' '-' '_' '.' '$']*
let lf_name = ['a'-'z' 'A'-'Z' '_' '.'] iden    (* Label and function names. *)
let char = ['\032'-'\126']                      (* Valid characters. *)
let char_lit = '\'' char '\''                   (* Character literals. *)
let whitespace = [' ' '\t' '\r']+

rule read_token src = parse
    | whitespace        { read_token src lexbuf }
    | '\n'              { Lexing.new_line lexbuf; read_token src lexbuf }
    | "(*"              { comment src 1 lexbuf }
    | "constant"        { CONSTANT }
    | "temp"            { TEMP }
    | "static"          { STATIC }
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
    | "scall"           { SCALL }
    | "return"          { RETURN }
    | char_lit          { NUM (int_of_char (Lexing.lexeme lexbuf).[1]) }
    | digit             { NUM (int_of_string (Lexing.lexeme lexbuf)) }
    | lf_name           { ID (Lexing.lexeme lexbuf) }
    | eof               { EOF }
    | _                 { raise (cantmatch src lexbuf.lex_start_p) }
and comment src depth = parse
    | "(*"              { comment src (depth +1) lexbuf }
    | "*)"              { if depth = 1 then read_token src lexbuf
                         else comment src (depth-1) lexbuf }
    | eof               { raise (unterminated_comments
                                 src lexbuf.lex_start_p depth) }
    | '\n'              { Lexing.new_line lexbuf; comment src depth lexbuf }
    | _                 { comment src depth lexbuf }

