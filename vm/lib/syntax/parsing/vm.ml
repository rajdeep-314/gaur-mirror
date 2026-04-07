(*
    vm.ml

    Interface for lexing and parsing VM source code files.
*)


module E = Err
module F = Utils.Files
module P = Utils.Parsing


let parse_from_path path =
    let src = P.expand_tabs 4 (F.read_file path) in
    let lexbuf = Lexing.from_string src in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = path } ;
    try
    Vm_parser.prog Vm_lexer.read_token lexbuf
    with
    | E.LexingError e -> raise (E.LexingErrorWI ((src, lexbuf.lex_start_p), e))

