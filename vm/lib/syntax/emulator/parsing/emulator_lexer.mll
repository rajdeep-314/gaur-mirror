(*
    emulator_lexer.mll

    Lexer specification for the VM emulator programs.
*)


{
    open Emulator_parser
    open Err.Helper.Lexer
}

let digit = ['0'-'9']+
let iden = ['a'-'z' 'A'-'Z' '0'-'9' '-' '_' '.' '$']*
let lf_name = ['a'-'z' 'A'-'Z' '_' '.'] iden    (* Label and function names. *)
let char = ['\032'-'\126']                      (* Valid characters. *)
let char_lit = '\'' char '\''                   (* Character literals. *)
let whitespace = [' ' '\t' '\r']+


rule read_token = parse
    | whitespace        { read_token lexbuf }
    | '\n'              { Lexing.new_line lexbuf; read_token lexbuf }
    | "(*"              { comment 1 lexbuf }
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
    | "="               { REQ }
    | "<="              { RLE }
    | ">="              { RGE }
    | "<>"              { RNE }
    | "<"               { RLT }
    | ">"               { RGT }
    | "+"               { BADD }
    | "-"               { BSUB }
    | "*"               { BMUL }
    | "/"               { BDIV }
    | "&&"              { BLAND }
    | "||"              { BLOR }
    | "stack"           { STACK }
    | "("               { LPAREN }
    | ")"               { RPAREN }
    | "["               { LSQUARE }
    | "]"               { RSQUARE }
    | "true"            { TRUE }
    | "false"           { FALSE }
    | "~"               { UNOT }
    | "%l"              { LBPT }
    | "%c"              { CBPT }
    | char_lit          { NUM (int_of_char (Lexing.lexeme lexbuf).[1]) }
    | digit             { NUM (int_of_string (Lexing.lexeme lexbuf)) }
    | lf_name           { ID (Lexing.lexeme lexbuf) }
    | eof               { EOF }
    | _                 { raise (cantmatch Err.(pos_info_of_lexbuf lexbuf)) }
and comment depth = parse
    | "(*"              { comment (depth +1) lexbuf }
    | "*)"              { if depth = 1 then read_token lexbuf
                          else comment (depth-1) lexbuf }
    | eof               { raise (unterminated_comments depth Err.(pos_info_of_lexbuf lexbuf)) }
    | '\n'              { Lexing.new_line lexbuf; comment depth lexbuf }
    | _                 { comment depth lexbuf }

