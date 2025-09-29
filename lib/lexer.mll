{
    type token =
        | LLI | LUI | LD | ST | ADD | ADDI | SUB
        | AND | NOT | BEQ | BLT | JALR | APC | J
        | REG of int
        | IMM of int
        | NEWLINE
        | EOF
}


let whitespace = [' ' '\t' '\r']+
let digit = ['0'-'9']
let natural = digit+
let integer = '-'? natural
let reg = 'r' natural
let comment = '#' [^'\n']*


rule token = parse
    | whitespace        { token lexbuf }
    | comment           { token lexbuf }
    | '\n'              { NEWLINE }
    | "lli"             { LLI }
    | "lui"             { LUI }
    | "ld"              { LD }
    | "st"              { ST }
    | "add"             { ADD }
    | "addi"            { ADDI }
    | "sub"             { SUB }
    | "and"             { AND }
    | "not"             { NOT }
    | "beq"             { BEQ }
    | "blt"             { BLT }
    | "jalr"            { JALR }
    | "apc"             { APC }
    | "j"               { J }
    | reg               { let s = Lexing.lexeme lexbuf in
                        REG (int_of_string (String.sub s 1 (String.length s - 1))) }
    | integer            { IMM (int_of_string (Lexing.lexeme lexbuf)) }
    | eof               { EOF }
    | _                 { failwith ("Lexing error: Match failed for" ^ Lexing.lexeme lexbuf) }

