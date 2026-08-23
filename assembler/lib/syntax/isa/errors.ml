(** Errors encountered while lexing and parsing. *)


(** Position info: starting and ending lexing positions. *)
type pos_info = Lexing.position * Lexing.position

(** Lexing errors. *)
module LexingErrors = struct
    (** Base errors. *)
    type base =
        | CantMatch of string

    (** Errors with info about the position where it occurred. *)
    type t = pos_info * base
end

(** Parsing errors. *)
module ParsingErrors = struct
    (** Base errors. *)
    type base =
        | InvalidRegNum of int

    (** Errors with info about the position where it occurred. *)
    type t = pos_info * base
end


(** Exceptions corresponding to lexing and parsing errors. *)
type exn +=
    | LexingExn  of LexingErrors.t
    | ParsingExn of ParsingErrors.t


(** Helper functions to raise exceptions. *)
module Helper = struct
    (** Lexing exceptions. *)
    module LexingExns = struct
        open LexingErrors
        (** Lexing exception from lexing error and pos_info. *)
        let of_err_and_ps (e:base) (ps:pos_info) = raise (LexingExn (ps, e))

        let cantmatch s ps = of_err_and_ps (CantMatch s) ps
    end

    (** Parsing exceptions. *)
    module ParsingExns = struct
        open ParsingErrors
        (** Parsing exception from parsing error and pos_info. *)
        let of_err_and_ps (e:base) (ps:pos_info) = raise (ParsingExn (ps, e))

        let invalidregnum n ps = of_err_and_ps (InvalidRegNum n) ps
    end
end

