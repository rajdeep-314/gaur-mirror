(* Add comments. *)

module Bit :
    sig
        type t
        val zero : t
        val one : t
        type imm4
        type imm8
        type imm12

        val bin_to_int : t list -> int
        val imm4_to_int : imm4 -> int
        val imm8_to_int : imm8 -> int
        val imm12_to_int : imm12 -> int
    end


module Reg :
    sig
        type t =
            | R0 | R1 | R2 | R3
            | R4 | R5 | R6 | R7
            | R8 | R9 | R10 | R11
            | R12 | R13 | R14 | R15

        val to_int : t -> int
    end

type t = 
    | Lli of Reg.t * Bit.imm8
    | Lui of Reg.t * Bit.imm8
    | Ld of Reg.t * Reg.t * Bit.imm4
    | St of Reg.t * Reg.t * Bit.imm4
    | Add of Reg.t * Reg.t * Reg.t
    | Addi of Reg.t * Reg.t * Bit.imm4
    | Sub of Reg.t * Reg.t * Reg.t
    | And of Reg.t * Reg.t * Reg.t
    | Not of Reg.t * Reg.t
    | Beq of Reg.t * Reg.t * Bit.imm4
    | Blt of Reg.t * Reg.t * Bit.imm4
    | Jalr of Reg.t * Reg.t * Bit.imm4
    | Apc of Reg.t * Bit.imm8
    | J of Bit.imm12

