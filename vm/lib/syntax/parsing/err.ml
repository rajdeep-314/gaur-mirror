(*
    err.ml

    Parsing-related errors.
*)


module L = Lexing
module U = Utils.Parsing


(* Info related to the error: source code and lexing position. *)
type info = string * L.position

let string_of_info (src, l) = L.(
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


type exn +=
    | LexingError   of Lexer.t

(* Pretty-printer for lexing error exceptions. *)
let pp_lexing_error = function
    | LexingError e -> Some (Printf.sprintf
        "Lexing error: %s" (Lexer.to_string e))
    | _ -> None

(* Registering pp_lexing_error. *)
let () = Printexc.register_printer pp_lexing_error


(* Helper functions to raise exceptions. *)
module Helper = struct
    module Lexer = struct
        open Lexer

        let lexing_exn e = LexingError e

        let cantmatch = lexing_exn CantMatch
        let unterminated_comments n = lexing_exn (UnterminatedComments n)
    end
end

