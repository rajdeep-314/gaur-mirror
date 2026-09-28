module Types  = Types
module Ast    = Ast
module Parser = Parser
module Lexer  = Lexer
module Messages = Messages

open Hdlutils
open Common.Error

let create source summary message =
  Reporting.create
    ~kind:Reporting.parsing
    ~severity:Reporting.error
    ~code:(-1)
    ~source:source
    ~summary:summary
    ~message:message

let from_channel fname ic =
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };
  lexbuf

(** [parse s] parses the program from string [s]. *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [parse_ic fname ic] parses the program in file [fname] from in-channel [ic]. *)
let parse_ic fname ic =
  let lexbuf = from_channel fname ic in
  try
    let ast = Parser.prog Lexer.read lexbuf in
    ast
  with
  | Errors.SyntaxError s ->
    Fmt.(string stderr s);
    exit Exit.lexing_error

  | Errors.DuplicateKey (key, loc) ->
    Loc.error_fmt Fmt.stderr loc;
    Fmt.(pf stderr) "Duplicate key %s" key;
    exit Exit.lexing_error

  | Parser.Error st ->
    let open Lexing in
    let loc = Loc.make (lexeme_start_p lexbuf) (lexeme_end_p lexbuf) in
    let message = Messages.message st in
    let error = create loc message message in
    Reporting.pp_short Fmt.stderr error;
    exit Exit.parsing_error

(** [parse_ic_many [(fname, ic); ....]] parses all programs in files
    [fname,...] from the corresponding in-channels [ic, ...].
*)
let parse_ic_many fcs =
  fcs
  |> List.map (fun (fname, ic) -> parse_ic fname ic)
  |> List.flatten

