(*
    driver.ml

    Interface for lexing and parsing VM emulator source code files.
*)


module E = Err
module F = Utils.Files
module P = Utils.Parsing


(** [parse_with_path src path] parses string program [src], presumably from a
    file with path [path] and returns an AST ASM program annotated with a tuple
    of [Lexing.position], with string labels (i.e. the type [syn_prog]).

    @raise LexingExn if the lexing stage fails.
    @raise ParsingExn if the parsing stage fails (includes Menhir errors).
*)
let parse_with_path src path =
    let lexbuf = Lexing.from_string src in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = path } ;
    try
        Emulator_parser.prog Emulator_lexer.read_token lexbuf, (path, src)
    with
        (* For Menhir's parsing errors. *)
        | Emulator_parser.Error n ->
            E.Helper.Parser.menhir_error n (E.pos_info_of_lexbuf lexbuf)
        (* For our own lexing and parsing errors. *)
        | (E.LexingExn _ | E.ParsingExn _) as e -> raise e


(** [parse_from_path prog] parses string program [prog] from a file
    with path [path], expands tabs to four spaces and returns an AST ASM
    program annotated with a tuple of [Lexing.position], with string labels
    (i.e. the type [syn_prog]).

    Calls [parse_with_path] after reading the file and expanding tabs.

    @raise LexingExn if the lexing stage fails.
    @raise ParsingExn if the parsing stage fails (includes Menhir errors).
*)
let parse_from_path path =
    let src = P.expand_tabs 4 (F.read_file path) in
    parse_with_path src path

