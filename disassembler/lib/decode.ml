(*
    decode.ml
*)


module E = Errors
module UB = Utils.Bitwise
module UR = Utils.Results

open E.Helper
open Assembler.Types
open Assembler.Types.Helper.Isa


let int_to_reg = function
    | 0  -> R0  | 1  -> R1  | 2  -> R2  | 3  -> R3
    | 4  -> R4  | 5  -> R5  | 6  -> R6  | 7  -> R7
    | 8  -> R8  | 9  -> R9  | 10 -> R10 | 11 -> R11
    | 12 -> R12 | 13 -> R13 | 14 -> R14 | 15 -> R15
    | 16 -> R16 | 17 -> R17 | 18 -> R18 | 19 -> R19
    | 20 -> R20 | 21 -> R21 | 22 -> R22 | 23 -> R23
    | 24 -> R24 | 25 -> R25 | 26 -> R26 | 27 -> R27
    | 28 -> R28 | 29 -> R29 | 30 -> R30 | 31 -> R31
    | n  -> raise (Invalid_argument (
        Printf.sprintf "int_to_reg got %d, invalid register number" n))


let ensure_zero_bits instr hi lo =
    UB.get_bits instr hi lo = 0

let disassemble_inst offset instr =
    let gb = UB.get_bits in
    let gr hi lo = int_to_reg (gb instr hi lo) in
    let sext = UB.sign_extend in

    let opcode = gb instr 31 16 in
    match opcode with
    | 0x1 -> (*1*)
        let rd = gr 15 11 in
        let imm = gb instr 10 0 in
        Ok (li rd imm)

    | 0x2 -> (*2*)
        let rd = gr 15 11 in
        let rs = gr 10 6 in
        let imm = sext (gb instr 5 0) 6 in
        Ok (ld rd rs imm)

    | 0x4 -> (*3*)
        let rd = gr 15 11 in
        let rs = gr 10 6 in
        let imm = sext (gb instr 5 0) 6 in
        Ok (st rd rs imm)

    | 0x8 -> (*4*)
        if Bool.not (ensure_zero_bits instr 0 0) then
            invalid_zero_bits offset instr opcode
        else
            let rd = gr 15 11 in
            let rs1 = gr 10 6 in
            let rs2 = gr 5 1 in
            Ok (add rd rs1 rs2)

    | 0x10 -> (*5*)
        let rd = gr 15 11 in
        let rs = gr 10 6 in
        let imm = sext (gb instr 5 0) 6 in
        Ok (addi rd rs imm)

    | 0x20 -> (*6*)
        if Bool.not (ensure_zero_bits instr 0 0) then
            invalid_zero_bits offset instr opcode
        else
            let rd = gr 15 11 in
            let rs1 = gr 10 6 in
            let rs2 = gr 5 1 in
            Ok (sub rd rs1 rs2)

    | 0x40 -> (*7*)
        if Bool.not (ensure_zero_bits instr 0 0) then
            invalid_zero_bits offset instr opcode
        else
            let rd = gr 15 11 in
            let rs1 = gr 10 6 in
            let rs2 = gr 5 1 in
            Ok (and' rd rs1 rs2)

    | 0x80 ->
        if Bool.not (ensure_zero_bits instr 5 0) then
            invalid_zero_bits offset instr opcode
        else
            let rd = gr 15 11 in
            let rs = gr 10 6 in
            Ok (not rd rs)

    | 0x100 ->
        if Bool.not (ensure_zero_bits instr 5 5) then
            invalid_zero_bits offset instr opcode
        else
            let rd = gr 15 11 in
            let rs = gr 10 6 in
            let imm = sext (gb instr 5 0) 6 in
            Ok (sl rd rs imm)

    | 0x200 ->
        if Bool.not (ensure_zero_bits instr 5 5) then
            invalid_zero_bits offset instr opcode
        else
            let rd = gr 15 11 in
            let rs = gr 10 6 in
            let imm = sext (gb instr 5 0) 6 in
            Ok (sr rd rs imm)

    | 0x400 ->
        let rd = gr 15 11 in
        let rs = gr 10 6 in
        let imm = sext (gb instr 5 0) 6 in
        Ok (beq rd rs imm)

    | 0x800 ->
        let rd = gr 15 11 in
        let rs = gr 10 6 in
        let imm = sext (gb instr 5 0) 6 in
        Ok (blt rd rs imm)

    | 0x1000 ->
        let rd = gr 15 11 in
        let rs = gr 10 6 in
        let imm = sext (gb instr 5 0) 6 in
        Ok (jalr rd rs imm)

    | 0x2000 ->
        let rd = gr 15 11 in
        let imm = sext (gb instr 10 0) 11 in
        Ok (apc rd imm)

    | 0x4000 ->
        let imm = sext (gb instr 15 0) 16 in
        Ok (j imm)

    | _ ->
        invalid_op_code offset instr opcode

let disassemble_prog instrs =
    UR.flatten_results
        (List.mapi (fun offset instr -> disassemble_inst offset instr) instrs)

