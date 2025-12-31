(*
    lexer.mll

    Lexer for the textual RAM file.
*)


{
    open Parser
}


let whitespace = [' ' '\t' '\r']+

let dec_digit = ['0'-'9']
let dec_natural = dec_digit+
let dec_integer = '-'? dec_natural

let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let hex_natural = ("0x" | "0X") hex_digit+
let hex_integer = '-'? hex_natural

let bin_digit = ['0'-'1']
let bin_natural = ("0b" | "0B") dec_digit+
let bin_integer = '-'? bin_natural

let comment = '#' [^'\n']*


rule read_toks = parse
    | whitespace            { read_toks lexbuf }
    | comment               { read_toks lexbuf }
    | '\n'                  { NEWLINE }
    | bin_integer           { NUM (Big_int.big_int_of_string (Lexing.lexeme lexbuf)) }
    | hex_integer           { NUM (Big_int.big_int_of_string (Lexing.lexeme lexbuf)) }
    | dec_integer           { NUM (Big_int.big_int_of_string (Lexing.lexeme lexbuf)) }
    | ':'                   { COLON }
    | eof                   { EOF }
    | _                     { failwith ("Lexing error: Match failed for " ^ Lexing.lexeme lexbuf) }

