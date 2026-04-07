{
  open Lexing
  open Parser
  open Errors
}

let white     = [' ' '\t']+
let newline   = '\r' | '\n' | "\r\n"

let bin_digit = '0' | '1'
let binary    = ("0b" | "0B") (bin_digit+)

let oct_digit = ['0'-'7']
let octal     = ("0o" | "0O") (oct_digit+)

let digit     = ['0'-'9']
let int       = digit+

let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let hex       = ("0x" | "0X") (hex_digit+)

let num       = int

let lower     = ['a'-'z']
let upper     = ['A'-'Z']

(* lowercase identifier *)
let lid       = (lower | '_' | digit)+
(* PascalCase identifier *)
let pid       = upper (lower | '_' | digit)*

let prime     = "'" | "ʹ"

rule read =
  parse
  (* Ignore whitespaces and newlines *)
  | white     { read lexbuf }
  | newline   { new_line lexbuf; read lexbuf }

  (* Ignore single-line comments *)
  | '#'       { skipComment lexbuf }

  (* Language keywords *)
  | "bits"    { BITS   }
  | "CHIP"    { CHIP   }
  | "INPUT"   { INPUT  }
  | "OUTPUT"  { OUTPUT }
  | "STATES"  { STATES }

  (* Assignment operators *)
  | '='       { FIELD_ASSIGN }
  | ":="      { COMB_DEF     }
  | "<-"      { SEQ_DEF      }

  (* Operators *)
  | "++"      { PLUSPLUS }
  | '['       { LSQUARE  }
  | ']'       { RSQUARE  }
  | ':'       { COLON    }

  | '.'       { DOT    }
  | ','       { COMMA  }
  | '{'       { LBRACE }
  | '}'       { RBRACE }
  | '('       { LPAREN }
  | ')'       { RPAREN }

  | prime     { PRIME }

  | num       { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | lid       { LID (Lexing.lexeme lexbuf) }
  | pid       { PID (Lexing.lexeme lexbuf) }

  | eof       { EOF }

  | _         { raise (SyntaxError  ( Printf.sprintf
                                      "Unexpected char '%s'.\n"
                                      (Lexing.lexeme lexbuf)
                                    ))
              }

and skipComment =
(* Skip comment string until newline or EOF *)
  parse
  | '\n'      { new_line lexbuf; read lexbuf }
  | eof       { EOF }
  | _         { skipComment lexbuf }
