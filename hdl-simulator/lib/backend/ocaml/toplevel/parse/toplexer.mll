{
  open Lexing
  open Topparser
  open Bits
  open Errors

  let of_binary lexbuf : bv =
    lexbuf
    |> Lexing.lexeme
    |> String.to_seq
    |> Seq.map (function
       | '0' -> false
       | '1' -> true
       |  c  -> raise_unex_char_bv c lexbuf)
    |> Array.of_seq
}

let white     = [' ' '\t']+
let newline   = '\r' | '\n' | "\r\n"

let bin_digit = '0' | '1'
let binary    = (bin_digit+)

let digit     = ['0'-'9']

let lower     = ['a'-'z']
let upper     = ['A'-'Z']

let lid       = lower (lower | upper | '_' | digit)*

rule read =
  parse
  (* Ignore whitespaces and newlines *)
  | white     { read lexbuf }
  | newline   { new_line lexbuf; read lexbuf }

  (* Commands *)
  | "INPUT"   { INPUT  }
  | "OUTPUT"  { OUTPUT }
  | "CLOCK"   { CLOCK  }
  | "QUIT"    { QUIT   }

  (* Assignment operators *)
  | ','       { COMMA  }
  | '='       { FIELD_ASSIGN }

  (* Parentheses *)
  | '{'       { LBRACE }
  | '}'       { RBRACE }

  | binary    { BV (of_binary lexbuf) }

  | lid       { LID (Lexing.lexeme lexbuf) }

  | eof       { EOF }

  | _         { raise_unex_string (Lexing.lexeme lexbuf) lexbuf }
