(*
    driver.ml

    Interface for lexing and parsing VM emulator source code files.
*)


module E = Err
module F = Utils.Files
module P = Utils.Parsing


(* Parse from a path. Returns AST and (path, source code) tuple. *)
let parse_from_path path =
    let src = P.expand_tabs 4 (F.read_file path) in
    let lexbuf = Lexing.from_string src in
    let info = (src, lexbuf) in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = path } ;
    try
        Emulator_parser.prog Emulator_lexer.read_token lexbuf, (path, src)
    with
        (* For (our own) lexing errors. *)
        | E.LexingError e   -> raise (E.LexingErrorWI (info, e))
        (* For our own parsing errors. *)
        | E.ParsingError e  -> raise (E.ParsingErrorWI (info, e))
        (* For Menhir's parsing errors. *)
        | Emulator_parser.Error n ->
            raise (E.ParsingErrorWI (info, E.Parser.MenhirError n))

