(*
    err.ml

    Parsing-related errors.
*)


module L = Lexing
module U = Utils.Parsing


(* Info related to the error: source code and lexing buffer. *)
type info = string * L.lexbuf

let string_of_info (src, lexbuf) = L.(
    let l = lexbuf.lex_start_p in
    Printf.sprintf
    "In file \"%s\", line %d, column %d\n%s\n%s"
    l.pos_fname l.pos_lnum (l.pos_cnum - l.pos_bol + 1)
    (U.get_line src l.pos_lnum) (U.carat_string (l.pos_cnum - l.pos_bol + 1))
    )


(* Lexing errors. *)
module Lexer = struct
    type t =
        (* Can't lex anymore. *)
        | CantMatch
        (* n unterminated comments before EOF. *)
        | UnterminatedComments  of int

    let to_string = function
        | CantMatch ->
            "Unexpected character, can't match."
        | UnterminatedComments 1 ->
            "Expected 1 comment to terminate before EOF."
        | UnterminatedComments n -> Printf.sprintf
            "Expected %d comments to terminate before EOF." n
end


module Parser = struct
    type t =
        | MenhirError       of int
        | InvalidSyscall    of string

    let to_string = function
        | MenhirError n     -> Messages.message n
        | InvalidSyscall s  -> s ^ " is not a valid system call name."
end


(* These exceptions have with-info and a without-info variants. The info comes
   from the driver code. *)
type exn +=
    | LexingError       of Lexer.t
    | LexingErrorWI     of info * Lexer.t
    | ParsingError      of Parser.t
    | ParsingErrorWI    of info * Parser.t


(* Pretty-printer for lexing error exceptions. *)
let pp_lexing_error_wi = function
    | LexingErrorWI (i, e) -> Some (Printf.sprintf
        "%s\nLexing error: %s\n" (string_of_info i) (Lexer.to_string e))
    | _ -> None

(* Registering pp_lexing_error_wi. *)
let () = Printexc.register_printer pp_lexing_error_wi


(* Pretty-printer for parser error exceptions. *)
let pp_parsing_error_wi = function
    | ParsingErrorWI (i, e) -> Some (Printf.sprintf
        "%s\nParsing error: %s" (string_of_info i) (Parser.to_string e))
    | _ -> None

(* Registering pp_parsing_error_wi. *)
let () = Printexc.register_printer pp_parsing_error_wi


(* Helper functions to raise exceptions. *)
module Helper = struct
    module Lexer = struct
        open Lexer

        let lexing_exn e = LexingError e

        let cantmatch = lexing_exn CantMatch
        let unterminated_comments n = lexing_exn (UnterminatedComments n)
    end

    module Parser = struct
        open Parser

        let invalid_syscall s = ParsingError (InvalidSyscall s)
    end
end

