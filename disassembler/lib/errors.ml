(*
    errors.ml
*)


(** {1 Disassembler errors} *)

let zero_bits_layout = function
    | 0x8   -> "h<04>_rd(5)_rs1(5)_rs2(5)_0  : opcode 0x08 requires bit 0 to be zero"
    | 0x20  -> "h<06>_rd(5)_rs1(5)_rs2(5)_0  : opcode 0x20 requires bit 0 to be zero"
    | 0x40  -> "h<07>_rd(5)_rs1(5)_rs2(5)_0  : opcode 0x40 requires bit 0 to be zero"
    | 0x80  -> "h<08>_rd(5)_rs1(5)_000000 : opcode 0x80 requires bits [5:0] must be zero"
    | 0x100 -> "h<09>_rd(5)_rs(5)_0_imm(5)  : opcode 0x100 requires bit 5 to be zero"
    | 0x200 -> "h<10>_rd(5)_rs(5)_0_imm(5)  : opcode 0x200 requires bit 5 to be zero"
    | n     -> raise (Invalid_argument (
        Printf.sprintf "No zero-bit layout for instruction with opcode %x" n))


(** Type for errors that the disassembler can produce. *)
type err =
    (* (offset, instruction, opcode) *)
    | InvalidOpcode     of int * int * int
    | InvalidZeroBits   of int * int * int


(** Printer functions for errors. *)
module Printers = struct
    (** String representation of errors. *)
    let int_to_hex n = Printf.sprintf "0x%08X" n

    let err = function
        | InvalidOpcode (offset, inst, op) -> Printf.sprintf
            "Invalid Opcode Error:\n  Inst   : %s\n  Offset : %d\n  Opcode : 0x%04X" (int_to_hex inst) offset op

        | InvalidZeroBits (offset, inst, op) -> Printf.sprintf
            "Invalid Zero Bit Error:\n  Inst     : %s\n  Offset   : %d\n  Expected : %s" (int_to_hex inst) offset (zero_bits_layout op)
end


module Helper = struct
    let invalid_op_code offset inst op =
        Error (InvalidOpcode (offset, inst, op))
    let invalid_zero_bits offset inst op =
        Error (InvalidZeroBits (offset, inst, op))
end

