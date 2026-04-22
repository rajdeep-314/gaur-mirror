(*
    decode.ml
*)


module E = Errors
module UB = Utils.Bitwise
module UR = Utils.Results

open Assembler_new.Types
open Assembler_new.Types.Helper


let int_to_reg = function
    | 0  -> Ok R0  | 1  -> Ok R1  | 2  -> Ok R2  | 3  -> Ok R3
    | 4  -> Ok R4  | 5  -> Ok R5  | 6  -> Ok R6  | 7  -> Ok R7
    | 8  -> Ok R8  | 9  -> Ok R9  | 10 -> Ok R10 | 11 -> Ok R11
    | 12 -> Ok R12 | 13 -> Ok R13 | 14 -> Ok R14 | 15 -> Ok R15
    | 16 -> Ok R16 | 17 -> Ok R17 | 18 -> Ok R18 | 19 -> Ok R19
    | 20 -> Ok R20 | 21 -> Ok R21 | 22 -> Ok R22 | 23 -> Ok R23
    | 24 -> Ok R24 | 25 -> Ok R25 | 26 -> Ok R26 | 27 -> Ok R27
    | 28 -> Ok R28 | 29 -> Ok R29 | 30 -> Ok R30 | 31 -> Ok R31
    | n  -> Error (E.InvalidRegNum n)


let ( let* ) = Result.bind

let disassemble_inst instr =
    let gb = UB.get_bits in
    let gr hi lo = int_to_reg (gb instr hi lo) in
    let sext = UB.sign_extend in

    let opcode = gb instr 31 16 in
    match opcode with
    | 0x1 -> (*1*)
        let* rd = gr 15 11 in
        let imm = gb instr 10 0 in
        Ok (ili rd imm)

    | 0x2 -> (*2*)
        let* rd = gr 15 11 in
        let* rs = gr 10 6 in
        let imm = sext (gb instr 5 0) 6 in
        Ok (ild rd rs imm)

    | 0x4 -> (*3*)
        let* rd = gr 15 11 in
        let* rs = gr 10 6 in
        let imm = sext (gb instr 5 0) 6 in
        Ok (ist rd rs imm)

    | 0x8 -> (*4*)
        let* rd = gr 15 11 in
        let* rs1 = gr 10 6 in
        let* rs2 = gr 5 1 in
        Ok (iadd rd rs1 rs2)

    | 0x10 -> (*5*)
        let* rd = gr 15 11 in
        let* rs = gr 10 6 in
        let imm = sext (gb instr 5 0) 6 in
        Ok (iaddi rd rs imm)

    | 0x20 -> (*6*)
        let* rd = gr 15 11 in
        let* rs1 = gr 10 6 in
        let* rs2 = gr 5 1 in
        Ok (isub rd rs1 rs2)

    | 0x40 -> (*7*)
        let* rd = gr 15 11 in
        let* rs1 = gr 10 6 in
        let* rs2 = gr 5 1 in
        Ok (iand rd rs1 rs2)

    (* TODO:    Add check for the remaining bits being 0. *)
    | 0x80 ->
        let* rd = gr 15 11 in
        let* rs = gr 10 6 in
        Ok (inot rd rs)

    | 0x100 ->
        let* rd = gr 15 11 in
        let* rs = gr 10 6 in
        let imm = sext (gb instr 5 0) 6 in
        Ok (isl rd rs imm)

    | 0x200 ->
        let* rd = gr 15 11 in
        let* rs = gr 10 6 in
        let imm = sext (gb instr 5 0) 6 in
        Ok (isr rd rs imm)

    | 0x400 ->
        let* rd = gr 15 11 in
        let* rs = gr 10 6 in
        let imm = sext (gb instr 5 0) 6 in
        Ok (ibeq rd rs imm)

    | 0x800 ->
        let* rd = gr 15 11 in
        let* rs = gr 10 6 in
        let imm = sext (gb instr 5 0) 6 in
        Ok (iblt rd rs imm)

    | 0x1000 ->
        let* rd = gr 15 11 in
        let* rs = gr 10 6 in
        let imm = sext (gb instr 5 0) 6 in
        Ok (ijalr rd rs imm)

    | 0x2000 -> 
        let* rd = gr 15 11 in
        let imm = sext (gb instr 10 0) 6 in
        Ok (iapc rd imm)

    | 0x4000 -> 
        let imm = sext (gb instr 15 0) 16 in
        Ok (ij imm)

    | _ ->
        Error E.InvalidOpcode

let disassemble_prog instrs =
    UR.flatten_results (List.map disassemble_inst instrs)

