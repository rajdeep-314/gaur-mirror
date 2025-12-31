(*
    lexer.mll

    Lexer for the debug mode.
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

let bin_digit = ['0' '1']
let bin_natural = ("0b" | "0B") bin_digit+
let bin_integer = '-'? bin_natural

let register = 'r' dec_natural
let comment = '#' [^'\n']*


rule read_tok = parse
    | whitespace        { read_tok lexbuf }
    | comment           { read_tok lexbuf }
    | "help"            { HELP }
    | "set trace"       { SET_TRACE }
    | "get trace"       { GET_TRACE }
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
    | "print inst"      { PRINT_INST }
    | "print prog"      { PRINT_PROG }
    | dec_integer       { NUM (int_of_string (Lexing.lexeme lexbuf)) }
    | hex_integer       { NUM (int_of_string (Lexing.lexeme lexbuf)) }
    | bin_integer       { NUM (int_of_string (Lexing.lexeme lexbuf)) }
    | eof               { EOF }
    | _                 { failwith "Lexer error." }

