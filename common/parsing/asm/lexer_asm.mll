(*
    lexer.mll

    Lexer specification for PASM.
*)


{
    open Errors
    open Errors.Helper.LexingExns
    open Parser_asm
}


let whitespace = [' ' '\r']+
let ascii = ['a'-'z' 'A'-'Z' '0'-'9' '-' '_' '.' '$']*
let label_name = ['a'-'z' 'A'-'Z' '_' '.'] ascii

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


rule read_token = parse
    | "\n"              { Lexing.new_line lexbuf; read_token lexbuf }
    | "\t"              { notabs (pos_info_of_lexbuf lexbuf) }
    | whitespace        { read_token lexbuf }
    | "#"               { read_comment lexbuf }
    | ":"               { COLON }
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
    | "spin"            { SPIN }
    | reg               { let s = Lexing.lexeme lexbuf in
                          REG (int_of_string (String.sub s 1 (String.length s - 1))) }
    | label_name        { LABEL (Lexing.lexeme lexbuf) } 
    | dec_integer       { IMM (int_of_string (Lexing.lexeme lexbuf)) }
    | hex_integer       { IMM (int_of_string (Lexing.lexeme lexbuf)) }
    | bin_integer       { IMM (int_of_string (Lexing.lexeme lexbuf)) }
    | eof               { EOF }
    | _                 { cantmatch (pos_info_of_lexbuf lexbuf) }

and read_comment = parse
    | "\n"              { read_token lexbuf }
    | "\t"              { notabs (pos_info_of_lexbuf lexbuf) }
    | _                 { read_comment lexbuf }

