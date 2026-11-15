open Hdlutils
open Common
open Common.Error

exception DuplicateKey of string * Loc.t

exception LexerError of string

type parse_error =
  | Lexing_error of string
  | Duplicate_key of string * Loc.t
  | Syntax_error of Diagnostics.t
  | Source_not_found of string

exception Parse_error of parse_error

let make_syntax_error source summary message =
  Diagnostics.create
    ~kind:Diagnostics.parsing
    ~severity:Diagnostics.error
    ~code:(-1)
    ~source:source
    ~summary:summary
    ~message:message

let pp_parse_error ?lookup ?resolver ppf err =
  let resolver =
    match resolver, lookup with
    | Some r, _ -> r
    | None, Some l -> Source.of_string_lookup l
    | None, None -> Source.default_resolver
  in
  match err with
  | Lexing_error s ->
    Fmt.string ppf s
  | Duplicate_key (key, loc) ->
    Fmt.pf ppf "%aDuplicate key %s" Loc.error_fmt loc key
  | Syntax_error diag ->
    Diagnostics.pp_short_with resolver ppf diag
  | Source_not_found name ->
    Fmt.pf ppf "Source '%s' not found" name
