module Types  = Types
module Ast    = Ast
module Parser = Parser
module Lexer  = Lexer

(** [parse s] parses the program from string [s]. *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [parse_ic ic] parses the program from in-channel [ic]. *)
let parse_ic ic =
  let lexbuf = Lexing.from_channel ic in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
