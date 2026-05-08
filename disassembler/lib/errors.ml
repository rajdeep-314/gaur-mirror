(*
    errors.ml
*)


(** {1 Disassembler errors} *)


(** Type for errors that the disassembler can produce. *)
type err =
    | InvalidOpcode of int * int * int (** The instruction opcode doesn't correspond to a valid PISA instruction. *)
    | InvalidRegNum of int  (** The register index in the instruction is out of bounds. *)
    | InvalidZeroBits of int * int * string (** inst , offset and the expected format *)


(** Printer functions for errors. *)
module Printers = struct
    (** String representation of errors. *)
    let int_to_hex n = Printf.sprintf "0x%08X" n

    let err = function
        | InvalidOpcode (inst, opcode, offset) ->
            Printf.sprintf
            "Invalid Opcode Error:\n  Inst   : %s\n  Offset : %d\n  Opcode : %s" (int_to_hex inst) offset (int_to_hex opcode)

        | InvalidZeroBits (inst, offset, expected) ->
            Printf.sprintf
            "Invalid Zero Bit Error:\n  Inst     : %s\n  Offset   : %d\n  Expected : %s" (int_to_hex inst) offset expected

        | InvalidRegNum n ->
            Printf.sprintf "Error: Register number %d out of bounds" n
end

