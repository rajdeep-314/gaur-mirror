module Types    = Types
module Ast      = Ast
module Errors   = Errors
module Parser   = Parser
module Lexer    = Lexer
module Messages = Messages

open Hdlutils
open Common
open Common.Error

let from_channel fname ic =
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };
  lexbuf

let from_string fname s =
  let lexbuf = Lexing.from_string s in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };
  lexbuf

let parse_lexbuf lexbuf =
  try Parser.prog Lexer.read lexbuf
  with
  | Errors.LexerError s ->
    raise (Errors.Parse_error (Lexing_error s))
  | Errors.DuplicateKey (key, loc) ->
    raise (Errors.Parse_error (Duplicate_key (key, loc)))
  | Parser.Error st ->
    let open Lexing in
    let loc = Loc.make (lexeme_start_p lexbuf) (lexeme_end_p lexbuf) in
    let message = Messages.message st in
    let error = Errors.make_syntax_error loc message message in
    raise (Errors.Parse_error (Syntax_error error))

(** [parse_source src] parses a [Source.t], raising [Errors.Parse_error] on failure. *)
let parse_source (src : Source.t) =
  match src.origin with
  | Memory { content } ->
    from_string src.name content |> parse_lexbuf
  | Disk { path } ->
    In_channel.with_open_text path (fun ic ->
      from_channel src.name ic |> parse_lexbuf)

(** [parse_sources [src1; ...]] parses all programs in the given sources,
    raising [Errors.Parse_error] on lexing or syntax errors. *)
let parse_sources sources =
  List.concat_map parse_source sources

(** [parse_with ?resolver name] resolves [name] using [resolver] and parses it.
    Raises [Errors.Parse_error (Source_not_found name)] if unresolvable. *)
let parse_with ?(resolver = Source.default_resolver) name =
  match resolver name with
  | Some src -> parse_source src
  | None -> raise (Errors.Parse_error (Source_not_found name))

(** [parse_with_many ?resolver names] resolves and parses a list of source names. *)
let parse_with_many ?(resolver = Source.default_resolver) names =
  List.concat_map (parse_with ~resolver) names

(** [parse_string fname s] parses the program in string [s] with filename [fname],
    raising [Errors.Parse_error] on lexing or syntax errors. *)
let parse_string fname s =
  parse_source (Source.from_string ~name:fname s)

(** [parse_strings [(fname, s); ...]] parses all programs in the given named
    strings, raising [Errors.Parse_error] on lexing or syntax errors. *)
let parse_strings sources =
  sources
  |> List.map (fun (fname, s) -> Source.from_string ~name:fname s)
  |> parse_sources

(** [parse s] parses the program from string [s]. *)
let parse s =
  parse_string "<string>" s

(** [parse_ic fname ic] parses the program in file [fname] from in-channel [ic].
    Outputs diagnostics to stderr and exits on error (CLI mode). *)
let parse_ic fname ic =
  let lexbuf = from_channel fname ic in
  try parse_lexbuf lexbuf
  with
  | Errors.Parse_error (Lexing_error s) ->
    Fmt.(string stderr s);
    exit Exit.lexing_error
  | Errors.Parse_error (Duplicate_key (key, loc)) ->
    Loc.error_fmt Fmt.stderr loc;
    Fmt.(pf stderr) "Duplicate key %s" key;
    exit Exit.lexing_error
  | Errors.Parse_error (Syntax_error error) ->
    Diagnostics.pp_short Fmt.stderr error;
    exit Exit.parsing_error
  | Errors.Parse_error (Source_not_found name) ->
    Fmt.pf Fmt.stderr "Source '%s' not found\n" name;
    exit Exit.parsing_error

(** [parse_ic_many [(fname, ic); ....]] parses all programs in files
    [fname,...] from the corresponding in-channels [ic, ...].
*)
let parse_ic_many fcs =
  fcs
  |> List.map (fun (fname, ic) -> parse_ic fname ic)
  |> List.flatten
