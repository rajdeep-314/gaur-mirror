(*
  decode.ml
*)

open Assembler.Types
module E = Err
module U = Utils

let int_to_reg = function
  | 0  -> R0  | 1  -> R1  | 2  -> R2  | 3  -> R3
  | 4  -> R4  | 5  -> R5  | 6  -> R6  | 7  -> R7
  | 8  -> R8  | 9  -> R9  | 10 -> R10 | 11 -> R11
  | 12 -> R12 | 13 -> R13 | 14 -> R14 | 15 -> R15
  | _  -> failwith "Invalid register index"


let disassemble_inst instr =
  let gb = U.get_bits in
  let sext = U.sign_extend in
  let opcode = gb instr 15 12 in
  match opcode with
  | 0x0 ->
      let rd  = int_to_reg (gb instr 11 8) in
      let imm = gb instr 7 0 in
      Ok (I_lli (rd, imm))

  | 0x1 ->
      let rd  = int_to_reg (gb instr 11 8) in
      let imm = gb instr 7 0 in
      Ok (I_lui (rd, imm))

  | 0x2 ->
      let rd  = int_to_reg (gb instr 11 8) in
      let rs  = int_to_reg (gb instr 7 4) in
      let imm = sext (gb instr 3 0) 4 in
      Ok (I_ld (rd, rs, imm))

  | 0x3 ->
      let rs1 = int_to_reg (gb instr 11 8) in
      let rs2 = int_to_reg (gb instr 7 4) in
      let imm = sext (gb instr 3 0) 4 in
      Ok (I_st (rs1, rs2, imm))

  | 0x4 ->
      let rd  = int_to_reg (gb instr 11 8) in
      let rs1 = int_to_reg (gb instr 7 4) in
      let rs2 = int_to_reg (gb instr 3 0) in
      Ok (I_add (rd, rs1, rs2))

  | 0x5 ->
      let rd  = int_to_reg (gb instr 11 8) in
      let rs  = int_to_reg (gb instr 7 4) in
      let imm = sext (gb instr 3 0) 4 in
      Ok (I_addi (rd, rs, imm))

  | 0x6 ->
      let rd  = int_to_reg (gb instr 11 8) in
      let rs1 = int_to_reg (gb instr 7 4) in
      let rs2 = int_to_reg (gb instr 3 0) in
      Ok (I_sub (rd, rs1, rs2))

  | 0x7 ->
      let rd  = int_to_reg (gb instr 11 8) in
      let rs1 = int_to_reg (gb instr 7 4) in
      let rs2 = int_to_reg (gb instr 3 0) in
      Ok (I_and (rd, rs1, rs2))

  | 0x8 ->
      let rd  = int_to_reg (gb instr 11 8) in
      let rs  = int_to_reg (gb instr 7 4) in
      Ok (I_not (rd, rs))

  | 0x9 ->
      let rs1 = int_to_reg (gb instr 11 8) in
      let rs2 = int_to_reg (gb instr 7 4) in
      let imm = sext (gb instr 3 0) 4 in
      Ok (I_beq (rs1, rs2, imm))

  | 0xA ->
      let rs1 = int_to_reg (gb instr 11 8) in
      let rs2 = int_to_reg (gb instr 7 4) in
      let imm = sext (gb instr 3 0) 4 in
      Ok (I_blt (rs1, rs2, imm))

  | 0xB ->
      let rd  = int_to_reg (gb instr 11 8) in
      let rs  = int_to_reg (gb instr 7 4) in
      let imm = sext (gb instr 3 0) 4 in
      Ok (I_jalr (rd, rs, imm))

  | 0xC ->
      let rd  = int_to_reg (gb instr 11 8) in
      let imm = sext (gb instr 7 0) 8 in
      Ok (I_apc (rd, imm))

  | 0xD ->
      let imm = sext (gb instr 11 0) 12 in
      Ok (I_j imm)

  | _ ->
      Error E.InvalidOpcode

let disassemble_prog instrs =
  U.flatten_results (List.map disassemble_inst instrs)

