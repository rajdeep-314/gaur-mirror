{
  open Lexing
  open Parser

  exception SyntaxError of string

  let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum;
  }
}

let white   = [' ' '\t']+
let newline = ['\r' '\n' "\r\n"]

let bin_digit = '0' | '1'
let binary    = ("0b" | "0B") (bin_digit+)

let oct_digit = ['0'-'7']
let octal     = ("0o" | "0O") (oct_digit+)

let digit   = ['0'-'9']
let int     = digit+

let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let hex       = ("0x" | "0X") (hex_digit+)

let num       = binary | octal | int | hex

let lower   = ['a'-'z']

let ident   = lower+

rule read =
  parse
  | white     { read lexbuf }
  | newline   { next_line lexbuf; read lexbuf }

  | "BITS"    { BITS }
  | "CHIP"    { CHIP }
  | "INPUT"   { INPUT }
  | "OUTPUT"  { OUTPUT }
  | "STATES"  { STATES }

  | '='       { EQ }
  | '.'       { DOT }
  | ':'       { COLON }
  | ','       { COMMA }
  | '{'       { LEFT_BRACE }
  | '}'       { RIGHT_BRACE }
  | '('       { LEFT_PAREN }
  | ')'       { RIGHT_PAREN }
  | '['       { LEFT_BRACK }
  | ']'       { RIGHT_BRACK }

  | "++"      { PLUSPLUS }
  | ".."      { ELLIPSIS }
  | "'"       { PRIME }
  | ":="      { DEF }

  | num       { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | ident     { IDENT (Lexing.lexeme lexbuf) }

  | '#'       { skipComment lexbuf }

  | eof       { EOF }

  | _         { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and skipComment =
  parse
  | '\n'      { next_line lexbuf; read lexbuf }
  | eof       { EOF }
  | _         { skipComment lexbuf }
