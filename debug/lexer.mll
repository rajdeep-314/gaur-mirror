{
    type token =
        | HELP | SET_TRACE | ON | OFF
        | REG | REGS | REGISTER of int
        | MEM | STEP | STEPS | PRINT_PROG
        | NUM of int
        | QUIT
        | INVALID
        | EOF
}


let whitespace = [' ' '\t' '\r']+

let dec_digit = ['0'-'9']
let dec_natural = dec_digit+
let dec_integer = '-'? dec_natural

let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let hex_natural = ("0x" | "0X") hex_digit+
let hex_integer = '-'? hex_natural

let bin_digit = ['0' '1']
let bin_natural = ("0b" | "0B") bin_digit+
let bin_integer = '-'? bin_natural

let register = 'r' dec_natural
let comment = '#' [^'\n']*


rule token = parse
    | whitespace        { token lexbuf }
    | comment           { token lexbuf }
    | "help"            { HELP }
    | "set trace"       { SET_TRACE }
    | "on"              { ON }
    | "off"             { OFF }
    | "regs"            { REGS }
    | "reg"             { REG }
    | register          { let s = Lexing.lexeme lexbuf in
                        REGISTER (int_of_string (String.sub s 1 (String.length s - 1))) }
    | "mem"             { MEM }
    | "steps"           { STEPS }
    | "step"            { STEP }
    | "quit"            { QUIT }
    | "print prog"      { PRINT_PROG }
    | dec_integer       { NUM (int_of_string (Lexing.lexeme lexbuf)) }
    | hex_integer       { NUM (int_of_string (Lexing.lexeme lexbuf)) }
    | bin_integer       { NUM (int_of_string (Lexing.lexeme lexbuf)) }
    | eof               { EOF }
    | _                 { INVALID }
    (* | _                 { failwith ("Lexing error: Match failed for " ^ Lexing.lexeme lexbuf) } *)


