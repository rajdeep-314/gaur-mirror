(*
    repl.ml

    Parser for REPL commands.
*)


module E = Err
module L = Lexing
module P = Repl_parser
module U = Utils.Parsing


(* String equivalent of an error for a given line, lexer buffer and error
   string. *)
let to_string line lexbuf err_str =
    let l = lexbuf.L.lex_start_p in
    let carat = U.carat_string (l.pos_cnum - l.pos_bol + 1) in Printf.sprintf
    "In command:\n%s\n%s\n%s"
    line carat err_str


(*
Read a line from stdin and return a value of type (cmd, string) result.
    - Ok c denotes that the input was successfully parsed to produce command c.
    - Error (Msg s) denotes a parsing error with s being the error string.
    - Error EOF denotes an End_of_file exception while reading for input
      (Ctrl + D, for example).
*)

type err =
    | Msg of string
    | EOF

let get_cmd () =
    try
        let inp = read_line () in
        let lexbuf = Lexing.from_string inp in
        let string_of_err = to_string inp lexbuf in
        try
            Ok (Repl_parser.cmd Repl_lexer.read_tokens lexbuf)
        with
        | E.LexingError _ as e ->
            Error (Msg (string_of_err (Printexc.to_string e)))
        | P.Error n ->
            Error (Msg (string_of_err (Messages.message n)))
    with
    | End_of_file -> Error EOF

