(*
    err.ml

    Lexing and parsing errors for REPL commands.
*)


module Lexer = struct
    type t =
        | UnmatchedCharacter

    let of_string = function
        | UnmatchedCharacter -> "Can't match characters from here"
end


type exn +=
    | LexingError   of Lexer.t


let pp_lexing_error = function
    | LexingError e -> Some (Printf.sprintf
        "Lexing error: %s." (Lexer.of_string e))
    | _ -> None

let () = Printexc.register_printer pp_lexing_error


module Helper = struct
    module Lexer = struct
        open Lexer

        let unmatched_character = LexingError UnmatchedCharacter
    end
end

