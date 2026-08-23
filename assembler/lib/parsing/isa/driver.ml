(** Driver code for (lexing and) parsing ISA programs. *)


open Errors
open Utils.Files


(** [parse_prog_with_path path prog] parses string program [prog] from a file
    with path [path] and returns an AST ISA program annotated with a tuple of
    [Lexing.position] (i.e. the type [syn_prog]).

    @raise LexingExn if the lexing stage fails.
    @raise ParsingExn if the parsing stage fails.
*)
let parse_prog_with_path path prog =
    let buf = Lexing.from_string prog in
    Lexing.set_filename buf path ;
    Parser_isa.prog Lexer_isa.read_token buf


(** [parse_prog_from_path path prog] parses program present at path [path] and
    returns an AST ISA program annotated with a tuple of [Lexing.position]
    (i.e. the type [syn_prog]).

    @raise LexingExn if the lexing stage fails.
    @raise ParsingExn if the parsing stage fails.
*)
let parse_prog_from_path path =
    let prog = read_file path in
    parse_prog_with_path path prog

