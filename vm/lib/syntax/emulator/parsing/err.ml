(*
    err.ml

    Parsing-related errors.
*)


module L = Lexing
module U = Utils.Parsingerrs

(** Position info: starting and ending lexing positions. *)
type pos_info = Lexing.position * Lexing.position

let pos_info_of_lexbuf buf : pos_info =
    (Lexing.lexeme_start_p buf, Lexing.lexeme_end_p buf)

(* Lexing errors. *)
module Lexer = struct
    type base =
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

    (** Errors with info about the position where it occurred. *)
    type t = pos_info * base

    (** Convert an error (a value of type [t]) to a string with the source
        program being [src]. *)
    let to_string_with_src ((ps, e):t) src =
        Printf.sprintf "Lexing error:\n%s\n%s"
        (U.pp_pos_info_with_source ~source:src ps)
        (to_string e)

    (** [fname_from_t e] returns the file name corresponding to the [t] value
        [e]. *)
    let fname_from_t : t -> string = fun ((p1, _), _) -> p1.pos_fname
end


module Parser = struct
    type base =
        | MenhirError         of int
        | InvalidSyscall      of string
        | OutOfBoundsPushC    of int
        | OutOfBoundsExpConst of int

    let to_string = function
        | MenhirError n         -> Messages.message n
        | InvalidSyscall s      -> s ^ " is not a valid system call name."
        | OutOfBoundsPushC n    -> Printf.sprintf
            "'push constant' value %d is outside int32's range." n
        | OutOfBoundsExpConst n -> Printf.sprintf
            "Sub-expression %d is outside int32's range." n

    (** Errors with info about the position where it occurred. *)
    type t = pos_info * base

    (** Convert an error (a value of type [t]) to a string with the source
        program being [src]. *)
    let to_string_with_src ((ps, e):t) src =
        Printf.sprintf "Parsing error:\n%s\n%s"
        (U.pp_pos_info_with_source ~source:src ps)
        (to_string e)

    (** [fname_from_t e] returns the file name corresponding to the [t] value
        [e]. *)
    let fname_from_t : t -> string = fun ((p1, _), _) -> p1.pos_fname
end


(* These exceptions have with-info and a without-info variants. The info comes
   from the driver code. *)
type exn +=
    | LexingExn     of Lexer.t
    | ParsingExn    of Parser.t


(* Helper functions to raise exceptions. *)
module Helper = struct
    module Lexer = struct
        open Lexer

        (** Lexing exception from lexing error and pos_info. *)
        let of_err_and_ps (e:base) (ps:pos_info) = raise (LexingExn (ps, e))

        let cantmatch ps = of_err_and_ps CantMatch ps
        let unterminated_comments n ps = of_err_and_ps (UnterminatedComments n) ps
    end

    module Parser = struct
        open Parser

        (** Parsing exception from parsing error and pos_info. *)
        let of_err_and_ps (e:base) (ps:pos_info) = raise (ParsingExn (ps, e))

        let menhir_error n ps = of_err_and_ps (MenhirError n) ps
        let invalid_syscall s ps = of_err_and_ps (InvalidSyscall s) ps
        let oob_pushc n ps = of_err_and_ps (OutOfBoundsPushC n) ps
        let oob_exp_const n ps = of_err_and_ps (OutOfBoundsExpConst n) ps
    end
end

