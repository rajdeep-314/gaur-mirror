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
    | R16 | R17 | R18 | R19
    | R20 | R21 | R22 | R23
    | R24 | R25 | R26 | R27
    | R28 | R29 | R30 | R31


type t = T.Isa.inst =
    | Li   of  reg * int
    | Ld    of  reg * reg * int
    | St    of  reg * reg * int
    | Add   of  reg * reg * reg
    | Addi  of  reg * reg * int
    | Sub   of  reg * reg * reg
    | And   of  reg * reg * reg
    | Not   of  reg * reg
    | Sl    of  reg * reg * int
    | Sr    of  reg * reg * int
    | Beq   of  reg * reg * int
    | Blt   of  reg * reg * int
    | Jalr  of  reg * reg * int
    | Apc   of  reg * int
    | J     of  int

type prog = t list


module Helper = struct
    (* Instructions. *)
    let li rd imm = Li (rd, imm)
    let ld rd rs imm = Ld (rd, rs, imm)
    let st rs1 rs2 imm = St (rs1, rs2, imm)
    let add rd rs1 rs2 = Add (rd, rs1, rs2)
    let addi rd rs imm = Addi (rd, rs, imm)
    let sub rd rs1 rs2 = Sub (rd, rs1, rs2)
    let and_ rd rs1 rs2 = And (rd, rs1, rs2)
    let not rd rs = Not (rd, rs)
    let sl rd rs imm = Sl  (rd,rs,imm)
    let sr rd rs imm = Sr  (rd,rs,imm) 
    let beq rs1 rs2 imm = Beq (rs1, rs2, imm)
    let blt rs1 rs2 imm = Blt (rs1, rs2, imm)
    let jalr rd rs imm = Jalr (rd, rs, imm)
    let apc rd imm = Apc (rd, imm)
    let j imm = J imm


    (* Some useful functions. *)
    let reg_to_int = function
        | R0 -> 0   | R1 -> 1  | R2 -> 2    | R3 -> 3
        | R4 -> 4   | R5 -> 5  | R6 -> 6    | R7 -> 7
        | R8 -> 8   | R9 -> 9  | R10 -> 10  | R11 -> 11
        | R12 -> 12 | R13 -> 13 | R14 -> 14 | R15 -> 15
        | R16 -> 16  | R17 -> 17 | R18 -> 18 | R19 -> 19
        | R20 -> 20  | R21 -> 21 | R22 -> 22 | R23 -> 23
        | R24 -> 24 | R25 -> 25  | R26 -> 26 | R27 -> 27
        | R28 -> 28 | R29 -> 29  | R30 -> 30 | R31 -> 31

    let reg_list = [R0; R1; R2; R3; R4; R5; R6; R7; R8; R9; R10; R11; R12; R13; R14; R15;
                    R16;R17;R18;R19;R20;R21;R22;R23;R24;R25;R26;R27;R28;R29;R30;R31]

    let reg_of_int n =
        if n < 0 || n > 15 then
            failwith "Error: Invalid register index. Must be in [0, 31]."
        else 
            List.nth reg_list n
end

