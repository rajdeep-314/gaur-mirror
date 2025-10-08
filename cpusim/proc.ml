(*
To do:
    - Make all this cleaner. Delegate work to helper functions.
    - Fetching the instruction itself in step? A wrapper function for that?
    - imm to int conversion functions for signed and unsigned immediates?
      Pass that as an argument? Can have default arguments.
*)

open Syntax.Ast

module Rf = Regfile

type pc = int
type t =
    | State of pc * Regfile.t * Ram.t

let create () = State (0, Regfile.create (), Ram.create ())

let get_pc (State (pc, _, _)) = pc
let get_regfile (State (_, rf, _)) = rf
let get_ram (State (_, _, ram)) = ram


(* Set the lower 8 bits of data to new_bits. *)
let set_lower data new_bits = (data land 0xff00) lor new_bits

(* Set the upper 8 bits of data to new_bits. *)
let set_upper data new_bits = (data land 0x00ff) lor (new_bits lsl 8)


let step_inst (State (pc, rf, ram)) inst =
    match inst with
    | Lli (rd, imm) ->
            let data = Rf.read rf rd in
            let data' = set_lower data (Bit.imm8_to_int imm) in
            let () = Rf.write rf rd data' in
            State (pc + 1, rf, ram)
    | Lui (rd, imm) ->
            let data = Rf.read rf rd in
            let data' = set_upper data (Bit.imm8_to_int imm) in
            let () = Rf.write rf rd data' in
            State (pc + 1, rf, ram)
    | Ld (rd, rs, imm) ->
            let base_addr = Rf.read rf rs in
            let offset = Bit.sign_extend (Bit.imm4_to_int imm) 4 in
            let mem_data = Ram.read ram (base_addr + offset) in
            let () = Rf.write rf rd mem_data in
            State (pc + 1, rf, ram)
    | St (rs1, rs2, imm) ->
            let base_addr = Rf.read rf rs2 in
            let offset = Bit.sign_extend (Bit.imm4_to_int imm) 4 in
            let data = Rf.read rf rs1 in
            let () = Ram.write ram (base_addr + offset) data in
            State (pc + 1, rf, ram)
    | Add (rd, rs1, rs2) ->
            let v1 = Rf.read rf rs1 in
            let v2 = Rf.read rf rs2 in
            let () = Rf.write rf rd (v1 + v2) in
            State (pc + 1, rf, ram)
    | Addi (rd, rs, imm) ->
            let rsv = Rf.read rf rs in
            let imm_sext = Bit.sign_extend (Bit.imm4_to_int imm) 4 in
            let () = Rf.write rf rd (rsv + imm_sext) in
            State (pc + 1, rf, ram)
    | Sub (rd, rs1, rs2) ->
            let v1 = Rf.read rf rs1 in
            let v2 = Rf.read rf rs2 in
            let () = Rf.write rf rd (v1 - v2) in
            State (pc + 1, rf, ram)
    | And (rd, rs1, rs2) ->
            let v1 = Rf.read rf rs1 in
            let v2 = Rf.read rf rs2 in
            let () = Rf.write rf rd (v1 land v2) in
            State (pc + 1, rf, ram)
    | Not (rd, rs) ->
            let rsv = Rf.read rf rs in
            let () = Rf.write rf rd (lnot rsv) in
            State (pc + 1, rf, ram)
    | Beq (rs1, rs2, imm) ->
            let v1 = Rf.read rf rs1 in
            let v2 = Rf.read rf rs2 in
            let imm_sext = Bit.sign_extend (Bit.imm4_to_int imm) 4 in
            if v1 == v2 then
                State (pc + imm_sext, rf, ram)
            else
                State (pc + 1, rf, ram)
    | Blt (rs1, rs2, imm) ->
            let v1 = Rf.read rf rs1 in
            let v2 = Rf.read rf rs2 in
            let v1_sext = Bit.sign_extend v1 16 in
            let v2_sext = Bit.sign_extend v2 16 in
            let imm_sext = Bit.sign_extend (Bit.imm4_to_int imm) 4 in
            if v1_sext < v2_sext then
                State (pc + imm_sext, rf, ram)
            else
                State (pc + 1, rf, ram)
    | Jalr (rd, rs, imm) ->
            let () = Rf.write rf rd (pc + 1) in
            let imm_sext = Bit.sign_extend (Bit.imm4_to_int imm) 4 in
            State (pc + imm_sext, rf, ram)
    | Apc (rd, imm) ->
            let rdv = Rf.read rf rd in
            let imm_sext = Bit.sign_extend (Bit.imm8_to_int imm) 8 in
            let () = Rf.write rf rd (rdv + pc + imm_sext) in
            State (pc + 1, rf, ram)
    | J imm ->
            let imm_sext = Bit.sign_extend (Bit.imm12_to_int imm) 12 in
            State (pc + imm_sext, rf, ram)


let step_prog (State (pc, rf, ram) as st) prog =
    match List.nth_opt prog pc with
    | None -> None
    | Some inst ->
            Some (step_inst st inst)


let rec execute_at_pc (State (pc, rf, ram) as st) prog =
    match List.nth_opt prog pc with
    | None -> st
    | Some inst ->
            execute_at_pc (step_inst st inst) prog

let execute_prog prog =
    let init_st = create () in
    execute_at_pc init_st prog

let execute_prog_on_ram ram prog =
    let init_st = State (0, Rf.create (), ram) in
    execute_at_pc init_st prog


(* Execution with traces. *)

let step_inst_wt (State (pc, rf, ram)) inst =
    match inst with
    | Lli (rd, imm) ->
            let data = Rf.read rf rd in
            let data' = set_lower data (Bit.imm8_to_int imm) in
            let () = Rf.write rf rd data' in
            (State (pc + 1, rf, ram), Trace.RegMod (pc, inst, rd, data'))
    | Lui (rd, imm) ->
            let data = Rf.read rf rd in
            let data' = set_upper data (Bit.imm8_to_int imm) in
            let () = Rf.write rf rd data' in
            (State (pc + 1, rf, ram), Trace.RegMod (pc, inst, rd, data'))
    | Ld (rd, rs, imm) ->
            let base_addr = Rf.read rf rs in
            let offset = Bit.sign_extend (Bit.imm4_to_int imm) 4 in
            let mem_data = Ram.read ram (base_addr + offset) in
            let () = Rf.write rf rd mem_data in
            (State (pc + 1, rf, ram), Trace.RegMod (pc, inst, rd, mem_data))
    | St (rs1, rs2, imm) ->
            let base_addr = Rf.read rf rs2 in
            let offset = Bit.sign_extend (Bit.imm4_to_int imm) 4 in
            let addr = base_addr + offset in
            let data = Rf.read rf rs1 in
            let () = Ram.write ram addr data in
            (State (pc + 1, rf, ram), Trace.RamMod (pc, inst, addr, data))
    | Add (rd, rs1, rs2) ->
            let v1 = Rf.read rf rs1 in
            let v2 = Rf.read rf rs2 in
            let sum = v1 + v2 in
            let () = Rf.write rf rd sum in
            (State (pc + 1, rf, ram), Trace.RegMod (pc, inst, rd, sum))
    | Addi (rd, rs, imm) ->
            let rsv = Rf.read rf rs in
            let imm_sext = Bit.sign_extend (Bit.imm4_to_int imm) 4 in
            let sum = rsv + imm_sext in
            let () = Rf.write rf rd sum in
            (State (pc + 1, rf, ram), Trace.RegMod (pc, inst, rd, sum))
    | Sub (rd, rs1, rs2) ->
            let v1 = Rf.read rf rs1 in
            let v2 = Rf.read rf rs2 in
            let diff = v1 - v2 in
            let () = Rf.write rf rd diff in
            (State (pc + 1, rf, ram), Trace.RegMod (pc, inst, rd, diff))
    | And (rd, rs1, rs2) ->
            let v1 = Rf.read rf rs1 in
            let v2 = Rf.read rf rs2 in
            let conj = v1 land v2 in
            let () = Rf.write rf rd conj in
            (State (pc + 1, rf, ram), Trace.RegMod (pc, inst, rd, conj))
    | Not (rd, rs) ->
            let rsv = Rf.read rf rs in
            let not = lnot rsv in
            let () = Rf.write rf rd not in
            (State (pc + 1, rf, ram), Trace.RegMod (pc, inst, rd, not))
    | Beq (rs1, rs2, imm) ->
            let v1 = Rf.read rf rs1 in
            let v2 = Rf.read rf rs2 in
            let imm_sext = Bit.sign_extend (Bit.imm4_to_int imm) 4 in
            let st' =
                if v1 == v2 then
                    State (pc + imm_sext, rf, ram)
                else
                    State (pc + 1, rf, ram) in
            (st', Trace.NoTrace (pc, inst))
    | Blt (rs1, rs2, imm) ->
            let v1 = Rf.read rf rs1 in
            let v2 = Rf.read rf rs2 in
            let v1_sext = Bit.sign_extend v1 16 in
            let v2_sext = Bit.sign_extend v2 16 in
            let imm_sext = Bit.sign_extend (Bit.imm4_to_int imm) 4 in
            let st' =
                if v1_sext < v2_sext then
                    State (pc + imm_sext, rf, ram)
                else
                    State (pc + 1, rf, ram) in
            (st', Trace.NoTrace (pc, inst))
    | Jalr (rd, rs, imm) ->
            let () = Rf.write rf rd (pc + 1) in
            let imm_sext = Bit.sign_extend (Bit.imm4_to_int imm) 4 in
            (State (pc + imm_sext, rf, ram), Trace.RegMod (pc, inst, rd, pc + 1))
    | Apc (rd, imm) ->
            let rdv = Rf.read rf rd in
            let imm_sext = Bit.sign_extend (Bit.imm8_to_int imm) 8 in
            let rdv' = rdv + pc + imm_sext in
            let () = Rf.write rf rd rdv' in
            (State (pc + 1, rf, ram), Trace.RegMod (pc, inst, rd, rdv'))
    | J imm ->
            let imm_sext = Bit.sign_extend (Bit.imm12_to_int imm) 12 in
            (State (pc + imm_sext, rf, ram), Trace.NoTrace (pc, inst))


let step_prog_wt (State (pc, rf, ram) as st) prog =
    match List.nth_opt prog pc with
    | None -> None
    | Some inst ->
            Some (step_inst_wt st inst)

(* tcs is the traces list. *)
let rec execute_at_pc_wt (State (pc, rf, ram) as st) prog tcs =
    match List.nth_opt prog pc with
    | None -> (st, List.rev tcs)
    | Some inst ->
            let (st', tc) = step_inst_wt st inst in
            execute_at_pc_wt st' prog (tc :: tcs)

let execute_prog_wt prog =
    let init_st = create () in
    execute_at_pc_wt init_st prog []

let execute_prog_on_ram_wt ram prog =
    let init_st = State (0, Rf.create (), ram) in
    execute_at_pc_wt init_st prog []


(* Executing and printing the trace. *)

let rec execute_at_pc_pt (State (pc, rf, ram) as st) prog =
    match List.nth_opt prog pc with
    | None ->
            let () = print_endline "\nHALT\n" in st
    | Some inst ->
            let (st', tc) = step_inst_wt st inst in
            let () = print_endline (Trace.to_string tc) in
            execute_at_pc_pt st' prog

let execute_prog_pt prog =
    let init_st = create () in
    execute_at_pc_pt init_st prog

let execute_prog_on_ram_pt ram prog =
    let init_st = State (0, Rf.create (), ram) in
    execute_at_pc_pt init_st prog


