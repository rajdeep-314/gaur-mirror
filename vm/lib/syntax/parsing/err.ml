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
    | LexingError   of info * Lexer.t

(* Pretty-printer for lexing error exceptions. *)
let pp_lexing_error = function
    | LexingError (i, e) -> Some (Printf.sprintf
        "%s\nLexing error: %s" (string_of_info i) (Lexer.to_string e))
    | _ -> None

(* Registering pp_lexing_error. *)
let () = Printexc.register_printer pp_lexing_error


(* Helper functions to raise exceptions. *)
module Helper = struct
    module Lexer = struct
        open Lexing

        (* LexingError exception for info (src, pos) and lexing error e. *)
        let lexing_exn src pos e = LexingError ((src, pos), e)

        let cantmatch src pos =
            lexing_exn src pos CantMatch
        let unterminated_comments src pos n =
            lexing_exn src pos (UnterminatedComments n)
    end
end

