(*
    decode.ml
*)


module E = Errors
module U = Utils

open Assembler.Types
open Assembler.Types.Helper


let int_to_reg = function
    | 0  -> Ok (R0)  | 1  -> Ok (R1)  | 2  -> Ok (R2)  | 3  -> Ok (R3)
    | 4  -> Ok (R4)  | 5  -> Ok (R5)  | 6  -> Ok (R6)  | 7  -> Ok (R7)
    | 8  -> Ok (R8)  | 9  -> Ok (R9)  | 10 -> Ok (R10) | 11 -> Ok (R11)
    | 12 -> Ok (R12) | 13 -> Ok (R13) | 14 -> Ok (R14) | 15 -> Ok (R15)
    | n  -> Error (E.InvalidRegNum n)


let ( let* ) = Result.bind

let disassemble_inst instr =
    let gb = U.get_bits in
    let gr hi lo = int_to_reg (gb instr hi lo) in
    let sext = U.sign_extend in

    (*let ( >>= ) r f =*)
    (*    match r with*)
    (*    | Ok x      -> f x*)
    (*    | Error e   -> Error e*)
    (*in*)

    let opcode = gb instr 15 12 in
    match opcode with
    | 0x0 ->
        let* rd = gr 11 8 in
        let imm = gb instr 7 0 in
        Ok (illi rd imm)

    | 0x1 ->
        let* rd = gr 11 8 in
        let imm = gb instr 7 0 in
        Ok (ilui rd imm)

    | 0x2 ->
        let* rd = gr 11 8 in
        let* rs = gr 7 4 in
        let imm = sext (gb instr 3 0) 4 in
        Ok (ild rd rs imm)

    | 0x3 ->
        let* rd = gr 11 8 in
        let* rs = gr 7 4 in
        let imm = sext (gb instr 3 0) 4 in
        Ok (ist rd rs imm)

    | 0x4 ->
        let* rd = gr 11 8 in
        let* rs1 = gr 7 4 in
        let* rs2 = gr 3 0 in
        Ok (iadd rd rs1 rs2)

    | 0x5 ->
        let* rd = gr 11 8 in
        let* rs = gr 7 4 in
        let imm = sext (gb instr 3 0) 4 in
        Ok (iaddi rd rs imm)

    | 0x6 ->
        let* rd = gr 11 8 in
        let* rs1 = gr 7 4 in
        let* rs2 = gr 3 0 in
        Ok (isub rd rs1 rs2)

    | 0x7 ->
        let* rd = gr 11 8 in
        let* rs1 = gr 7 4 in
        let* rs2 = gr 3 0 in
        Ok (iand rd rs1 rs2)

    (* TODO:    Add check for the remaining bits being 0. *)
    | 0x8 ->
        let* rd = gr 11 8 in
        let* rs = gr 7 4 in
        Ok (inot rd rs)

    | 0x9 ->
        let* rs1 = gr 11 8 in
        let* rs2 = gr 7 4 in
        let imm = sext (gb instr 3 0) 4 in
        Ok (ibeq rs1 rs2 imm)

    | 0xA ->
        let* rs1 = gr 11 8 in
        let* rs2 = gr 7 4 in
        let imm = sext (gb instr 3 0) 4 in
        Ok (iblt rs1 rs2 imm)

    | 0xB ->
        let* rs1 = gr 11 8 in
        let* rs2 = gr 7 4 in
        let imm = sext (gb instr 3 0) 4 in
        Ok (ijalr rs1 rs2 imm)

    | 0xC ->
        let* rd = gr 11 8 in
        let imm = sext (gb instr 7 0) 8 in
        Ok (iapc rd imm)

    | 0xD ->
        let imm = sext (gb instr 11 0) 12 in
        Ok (ij imm)

    | _ ->
        Error E.InvalidOpcode

let disassemble_prog instrs =
    U.flatten_results (List.map disassemble_inst instrs)

