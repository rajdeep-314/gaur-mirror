(*
    ast.ml

    Captures the abstract syntax of PASM.
*)


module Bit = struct
    type t = B0 | B1

    let zero = B0
    let one = B1

    type imm4 = t * t * t * t
    type imm8 = t * t * t * t * t * t * t * t
    type imm12 = t * t * t * t * t * t * t * t * t * t * t * t
end


module Reg = struct
    type t =
        | R0 | R1 | R2 | R3
        | R4 | R5 | R6 | R7
        | R8 | R9 | R10 | R11
        | R12 | R13 | R14 | R15

    let to_int = function
        | R0 -> 0 | R1 -> 1 | R2 -> 2 | R3 -> 3
        | R4 -> 4 | R5 -> 5 | R6 -> 6 | R7 -> 7
        | R8 -> 8 | R9 -> 9 | R10 -> 10 | R11 -> 11
        | R12 -> 12 | R13 -> 13 | R14 -> 14 | R15 -> 15
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



