(*
    errors.ml
*)


(** {1 Disassembler errors} *)


(** Type for errors that the disassembler can produce. *)
type err = 
    | InvalidOpcode  (** The instruction opcode doesn't correspond to a valid PISA instruction. *)
    | InvalidRegNum of int  (** The register index in the instruction is out of bounds. *)


(** Printer functions for errors. *)
module Printers = struct
    (** String representation of errors. *)
    let err = function
        | InvalidOpcode     -> "Error: Invalid Opcode"
        | InvalidRegNum n   -> Printf.sprintf "Error: Register number %d out of bounds" n
end

