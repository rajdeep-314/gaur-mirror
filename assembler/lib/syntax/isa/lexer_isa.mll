(*
    lexer.mll

    Lexer specification for PISA.
*)


{
    open Parser_isa
    open Errors.Helper.LexingExns

    let ps_of_lexbuf buf =
        (Lexing.lexeme_start_p buf, Lexing.lexeme_end_p buf)
}


let whitespace = [' ' '\t' '\r' '\n']+

let dec_digit = ['0'-'9']
let dec_natural = dec_digit+
let dec_integer = '-'? dec_natural

let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let hex_natural = ("0x" | "0X") hex_digit+
let hex_integer = '-'? hex_natural

let bin_digit = ['0' '1']
let bin_natural = ("0b" | "0B") bin_digit+
let bin_integer = '-'? bin_natural

let reg = 'r' dec_natural
let comment = '#' [^'\n']*


rule read_token = parse
    | whitespace        { read_token lexbuf }
    | comment           { read_token lexbuf }
    | "li"              { LI }
    | "ld"              { LD }
    | "st"              { ST }
    | "add"             { ADD }
    | "addi"            { ADDI }
    | "sub"             { SUB }
    | "and"             { AND }
    | "not"             { NOT }
    | "sl"              { SL }
    | "sr"              { SR }
    | "beq"             { BEQ }
    | "blt"             { BLT }
    | "jalr"            { JALR }
    | "apc"             { APC }
    | "j"               { J }
    | reg               { let s = Lexing.lexeme lexbuf in
                          REG (int_of_string (String.sub s 1 (String.length s - 1))) }
    | dec_integer       { IMM (int_of_string (Lexing.lexeme lexbuf)) }
    | hex_integer       { IMM (int_of_string (Lexing.lexeme lexbuf)) }
    | bin_integer       { IMM (int_of_string (Lexing.lexeme lexbuf)) }
    | eof               { EOF }
    | _                 { cantmatch (Lexing.lexeme lexbuf) (ps_of_lexbuf lexbuf) }

