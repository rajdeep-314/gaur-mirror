(** Driver code for (lexing and) parsing ASM programs. *)


open Errors
open Errors.Helper.ParsingExns
open Utils.Files


(** [parse_prog_with_path path prog] parses string program [prog] from a file
    with path [path] and returns an AST ASM program annotated with a tuple of
    [Lexing.position], with string labels (i.e. the type [syn_prog]).

    @raise Errors.LexingExn if the lexing stage fails.
    @raise Errors.ParsingExn if the parsing stage fails (includes Menhir errors).
*)
let parse_prog_with_path path prog =
    let buf = Lexing.from_string prog in
    Lexing.set_filename buf path ;
    try
        Parser_asm.prog Lexer_asm.read_token buf
    with
        | Parser_asm.Error n -> menhirerror n (pos_info_of_lexbuf buf)
        | (LexingExn _ | ParsingExn _) as e -> raise e


(** [parse_prog_from_path path prog] parses program present at path [path] and
    returns an AST ISA program annotated with a tuple of [Lexing.position],
    with string labels (i.e. the type [syn_prog]).

    @raise Errors.LexingExn if the lexing stage fails.
    @raise Errors.ParsingExn if the parsing stage fails.
*)
let parse_prog_from_path path =
    let prog = read_file path in
    parse_prog_with_path path prog
