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

        val sign_extend : int -> int -> int
    end


module Reg :
    sig
    type t =
        | R0 | R1 | R2 | R3
        | R4 | R5 | R6 | R7
        | R8 | R9 | R10 | R11
        | R12 | R13 | R14 | R15

    val to_int : t -> int
    val to_string : t -> string

    module Helper :
        sig
        val r0 : t
        val r1 : t
        val r2 : t
        val r3 : t
        val r4 : t
        val r5 : t
        val r6 : t
        val r7 : t
        val r8 : t
        val r9 : t
        val r10 : t
        val r11 : t
        val r12 : t
        val r13 : t
        val r14 : t
        val r15 : t
        val reg_list : t list
        end

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

type prog = t list


module Helper :
    sig
    val lli : Reg.t -> int -> t
    val lui : Reg.t -> int -> t
    val ld : Reg.t -> Reg.t -> int -> t
    val st  : Reg.t -> Reg.t -> int -> t
    val add : Reg.t -> Reg.t -> Reg.t -> t
    val addi : Reg.t -> Reg.t -> int -> t
    val sub : Reg.t -> Reg.t -> Reg.t -> t
    val and_ : Reg.t -> Reg.t -> Reg.t -> t
    val not : Reg.t -> Reg.t -> t
    val beq : Reg.t -> Reg.t -> int -> t
    val blt : Reg.t -> Reg.t -> int -> t
    val jalr : Reg.t -> Reg.t -> int -> t
    val apc : Reg.t -> int -> t
    val j : int -> t
    end

