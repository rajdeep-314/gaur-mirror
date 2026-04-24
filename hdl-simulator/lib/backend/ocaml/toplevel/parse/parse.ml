module Types    = Types
module Ast      = Ast
module Parser   = Topparser
module Lexer    = Toplexer
module Messages = Messages

open Hdlutils

let from_channel fname ic =
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };
  lexbuf

(** [parse s] parses the command from string [s]. *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.cmd Lexer.read lexbuf in
  ast

(** [parse_ic fname ic] parses the command in file [fname] from
    in-channel [ic].
*)
let parse_ic fname ic =
  let lexbuf = from_channel fname ic in
  let ast = Parser.cmd Lexer.read lexbuf in
  ast
