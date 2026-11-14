(** Errors encountered while lexing and parsing ASM programs. *)

module U = Utils.Parsingerrs


(** Position info: starting and ending lexing positions. *)
type pos_info = Lexing.position * Lexing.position

let pos_info_of_lexbuf buf : pos_info =
        (Lexing.lexeme_start_p buf, Lexing.lexeme_end_p buf)

(** Lexing errors. *)
module LexingErrors = struct
    (** Base errors. *)
    type base =
        | CantMatch
        | NoTabs

    let to_string = function
        | CantMatch -> Printf.sprintf "Can't match strings after this point"
        | NoTabs    -> Printf.sprintf "Tabs not allowed"

    (** Errors with info about the position where it occurred. *)
    type t = pos_info * base

    (** Convert an error (a value of type [t]) to a string with the source
        program being [src]. *)
    let to_string_with_src ((ps, e):t) src =
        Printf.sprintf "Lexing error:\n%s\n%s"
        (U.pp_pos_info_with_source ~source:src ps)
        (to_string e)
end

(** Parsing errors. *)
module ParsingErrors = struct
    (** Base errors.
        These errors contain:
            - Those that arise due to custom logic, such as invalid register number.
            - Those that arise due to Menhir -- grammatical issues.
            - Out-of-bound immediates where the expected immediate is:
                - 11-bit unsigned
                - 11-bit signed
                - 6-bit signed
                - 5-bit unsigned
    *)
    type base =
        | InvalidRegNum of int
        | MenhirError   of string
        | OOBimm11u     of int
        | OOBimm11s     of int
        | OOBimm6s      of int
        | OOBimm5u      of int

    let to_string = function
        | InvalidRegNum n -> Printf.sprintf "Invalid register index: %d" n
        | MenhirError s   -> s
        | OOBimm11u n     -> Printf.sprintf
            "Outside the 11-bit unsigned range [0, 2047]: %d" n
        | OOBimm11s n     -> Printf.sprintf
            "Outside the 11-bit signed range [-1024, 1023]: %d" n
        | OOBimm6s n      -> Printf.sprintf
            "Outside the 6-bit signed range [-32, 31]: %d" n
        | OOBimm5u n      -> Printf.sprintf
            "Outside the 5-bit unsigned range [0, 31]: %d" n

    (** Errors with info about the position where it occurred. *)
    type t = pos_info * base

    (** Convert an error (a value of type [t]) to a string with the source
        program being [src]. *)
    let to_string_with_src ((ps, e):t) src =
        Printf.sprintf "Parsing error:\n%s\n%s"
        (U.pp_pos_info_with_source ~source:src ps)
        (to_string e)
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

        let cantmatch ps = of_err_and_ps CantMatch ps
        let notabs ps = of_err_and_ps NoTabs ps
    end

    (** Parsing exceptions. *)
    module ParsingExns = struct
        open ParsingErrors
        (** Parsing exception from parsing error and pos_info. *)
        let of_err_and_ps (e:base) (ps:pos_info) = raise (ParsingExn (ps, e))

        let invalidregnum n ps = of_err_and_ps (InvalidRegNum n) ps
        let menhirerror n ps = of_err_and_ps (MenhirError (Messages.message n)) ps
        let oobimm11u n ps = of_err_and_ps (OOBimm11u n) ps
        let oobimm11s n ps = of_err_and_ps (OOBimm11s n) ps
        let oobimm6s n ps = of_err_and_ps (OOBimm6s n) ps
        let oobimm5u n ps = of_err_and_ps (OOBimm5u n) ps
    end
end

