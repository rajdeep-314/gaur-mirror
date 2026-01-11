(*
    ast.ml

    Captures the AST of PISA.

    NOTE:   Depends on assembler.
*)


module T = Assembler.Types

type reg = T.reg =
    | R0  | R1  | R2  | R3
    | R4  | R5  | R6  | R7
    | R8  | R9  | R10 | R11
    | R12 | R13 | R14 | R15


type t = T.isa_inst =
    | I_lli   of  reg * int
    | I_lui   of  reg * int
    | I_ld    of  reg * reg * int
    | I_st    of  reg * reg * int
    | I_add   of  reg * reg * reg
    | I_addi  of  reg * reg * int
    | I_sub   of  reg * reg * reg
    | I_and   of  reg * reg * reg
    | I_not   of  reg * reg
    | I_beq   of  reg * reg * int
    | I_blt   of  reg * reg * int
    | I_jalr  of  reg * reg * int
    | I_apc   of  reg * int
    | I_j     of  int

type prog = t list


module Helper = struct
    (* Instructions. *)
    let lli rd imm = I_lli (rd, imm)
    let lui rd imm = I_lui (rd, imm)
    let ld rd rs imm = I_ld (rd, rs, imm)
    let st rs1 rs2 imm = I_st (rs1, rs2, imm)
    let add rd rs1 rs2 = I_add (rd, rs1, rs2)
    let addi rd rs imm = I_addi (rd, rs, imm)
    let sub rd rs1 rs2 = I_sub (rd, rs1, rs2)
    let and_ rd rs1 rs2 = I_and (rd, rs1, rs2)
    let not rd rs = I_not (rd, rs)
    let beq rs1 rs2 imm = I_beq (rs1, rs2, imm)
    let blt rs1 rs2 imm = I_blt (rs1, rs2, imm)
    let jalr rd rs imm = I_jalr (rd, rs, imm)
    let apc rd imm = I_apc (rd, imm)
    let j imm = I_j imm


    (* Some useful functions. *)
    let reg_to_int = function
        | R0 -> 0   | R1 -> 1  | R2 -> 2    | R3 -> 3
        | R4 -> 4   | R5 -> 5  | R6 -> 6    | R7 -> 7
        | R8 -> 8   | R9 -> 9  | R10 -> 10  | R11 -> 11
        | R12 -> 12 | R13 -> 13 | R14 -> 14 | R15 -> 15

    let reg_list = [R0; R1; R2; R3; R4; R5; R6; R7; R8; R9; R10; R11; R12; R13; R14; R15]

    let reg_of_int n =
        if n < 0 || n > 15 then
            failwith "Error: Invalid register index. Must be in [0, 15]."
        else 
            List.nth reg_list n
end

