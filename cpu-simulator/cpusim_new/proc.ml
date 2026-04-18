(*
    proc.ml

    Simulating the processor.
*)

module Syntax = Syntax_new
open Syntax.Ast

module Rf = Regfile
module M = Utils.Bitwise

type pc = int
type t = State of pc * Regfile.t * Ram.t

let create () = State (0, Regfile.create (), Ram.create ())

let get_pc (State (pc, _, _)) = pc
let get_regfile (State (_, rf, _)) = rf
let get_ram (State (_, _, ram)) = ram


let step_inst (State (pc, rf, ram)) inst =
    match inst with
    | I_li (rd, imm) ->
            let () = Rf.write rf rd imm in
            State (pc + 1, rf, ram)

    | I_ld (rd, rs, imm) ->
            let base_addr = Rf.read rf rs in
            let offset = M.sign_extend imm 6 in
            let mem_data = Ram.read ram (base_addr + offset) in
            let () = Rf.write rf rd mem_data in
            State (pc + 1, rf, ram)

    | I_st (rs1, rs2, imm) ->
            let base_addr = Rf.read rf rs2 in
            let offset = M.sign_extend imm 6 in
            let data = Rf.read rf rs1 in
            let () = Ram.write ram (base_addr + offset) data in
            State (pc + 1, rf, ram)

    | I_add (rd, rs1, rs2) ->
            let v1 = Rf.read rf rs1 in
            let v2 = Rf.read rf rs2 in
            let () = Rf.write rf rd (v1 + v2) in
            State (pc + 1, rf, ram)

    | I_addi (rd, rs, imm) ->
            let rsv = Rf.read rf rs in
            let imm_sext = M.sign_extend imm 6 in
            let () = Rf.write rf rd (rsv + imm_sext) in
            State (pc + 1, rf, ram)

    | I_sub (rd, rs1, rs2) ->
            let v1 = Rf.read rf rs1 in
            let v2 = Rf.read rf rs2 in
            let () = Rf.write rf rd (v1 - v2) in
            State (pc + 1, rf, ram)

    | I_and (rd, rs1, rs2) ->
            let v1 = Rf.read rf rs1 in
            let v2 = Rf.read rf rs2 in
            let () = Rf.write rf rd (v1 land v2) in
            State (pc + 1, rf, ram)

    | I_not (rd, rs) ->
            let rsv = Rf.read rf rs in
            let () = Rf.write rf rd (lnot rsv) in
            State (pc + 1, rf, ram)

    | I_sl (rd, rs, imm) ->
            let rsv = Rf.read rf rs in
            let () = Rf.write rf rd (rsv lsl imm) in
            State (pc + 1, rf, ram)

    | I_sr (rd, rs, imm) ->
            let rsv = Rf.read rf rs in
            let () = Rf.write rf rd (rsv lsr imm) in
            State (pc + 1, rf, ram)

    | I_beq (rs1, rs2, imm) ->
            let v1 = Rf.read rf rs1 in
            let v2 = Rf.read rf rs2 in
            let imm_sext = M.sign_extend imm 6 in
            if v1 = v2 then
                State (pc + imm_sext, rf, ram)
            else
                State (pc + 1, rf, ram)

    | I_blt (rs1, rs2, imm) ->
            let v1 = Rf.read rf rs1 in
            let v2 = Rf.read rf rs2 in
            let v1_sext = M.sign_extend v1 32 in
            let v2_sext = M.sign_extend v2 32 in
            let imm_sext = M.sign_extend imm 6 in
            if v1_sext < v2_sext then
                State (pc + imm_sext, rf, ram)
            else
                State (pc + 1, rf, ram)

    | I_jalr (rd, rs, imm) ->
            let ret = pc + 1 in
            let imm_sext = M.sign_extend imm 6 in
            let target = (Rf.read rf rs) + imm_sext in
            let () = Rf.write rf rd ret in
            State (target, rf, ram)

    | I_apc (rd, imm) ->
            let rdv = Rf.read rf rd in
            let imm_sext = M.sign_extend imm 11 in
            let () = Rf.write rf rd (rdv + pc + imm_sext) in
            State (pc + 1, rf, ram)

    | I_j imm ->
            let imm_sext = M.sign_extend imm 16 in
            State (pc + imm_sext, rf, ram)


let step_prog (State (pc, rf, ram) as st) prog =
    match List.nth_opt prog pc with
    | None -> None
    | Some inst ->
            Some (step_inst st inst)


let rec execute_at_pc ?(max_steps = None) (State (pc, rf, ram) as st) prog =
    match max_steps with
    | None -> 
        begin
        match List.nth_opt prog pc with
        | None -> st
        | Some inst ->
                execute_at_pc (step_inst st inst) prog
        end
    | Some n ->
        if n <= 0 then st else
        match List.nth_opt prog pc with
        | None -> st
        | Some inst ->
                execute_at_pc ~max_steps:(Some (n - 1)) (step_inst st inst) prog

let execute_prog ?(max_steps = None) prog =
    let init_st = create () in
    execute_at_pc ~max_steps:max_steps init_st prog

let execute_prog_on_ram ?(max_steps = None) ram prog =
    let init_st = State (0, Rf.create (), ram) in
    execute_at_pc ~max_steps:max_steps init_st prog


(* Execution with traces. *)

let step_inst_wt (State (pc, rf, ram)) inst =
    match inst with
    | I_li (rd, imm) ->
            let () = Rf.write rf rd imm in
            (State (pc + 1, rf, ram), Trace.RegMod (pc, inst, rd, imm))

    | I_ld (rd, rs, imm) ->
            let base_addr = Rf.read rf rs in
            let offset = M.sign_extend imm 6 in
            let mem_data = Ram.read ram (base_addr + offset) in
            let () = Rf.write rf rd mem_data in
            (State (pc + 1, rf, ram), Trace.RegMod (pc, inst, rd, mem_data))

    | I_st (rs1, rs2, imm) ->
            let base_addr = Rf.read rf rs2 in
            let offset = M.sign_extend imm 6 in
            let addr = base_addr + offset in
            let data = Rf.read rf rs1 in
            let () = Ram.write ram addr data in
            (State (pc + 1, rf, ram), Trace.RamMod (pc, inst, addr, data))

    | I_add (rd, rs1, rs2) ->
            let sum = (Rf.read rf rs1) + (Rf.read rf rs2) in
            let () = Rf.write rf rd sum in
            (State (pc + 1, rf, ram), Trace.RegMod (pc, inst, rd, sum))

    | I_addi (rd, rs, imm) ->
            let sum = (Rf.read rf rs) + (M.sign_extend imm 6) in
            let () = Rf.write rf rd sum in
            (State (pc + 1, rf, ram), Trace.RegMod (pc, inst, rd, sum))

    | I_sub (rd, rs1, rs2) ->
            let diff = (Rf.read rf rs1) - (Rf.read rf rs2) in
            let () = Rf.write rf rd diff in
            (State (pc + 1, rf, ram), Trace.RegMod (pc, inst, rd, diff))

    | I_and (rd, rs1, rs2) ->
            let v = (Rf.read rf rs1) land (Rf.read rf rs2) in
            let () = Rf.write rf rd v in
            (State (pc + 1, rf, ram), Trace.RegMod (pc, inst, rd, v))

    | I_not (rd, rs) ->
            let v = lnot (Rf.read rf rs) in
            let () = Rf.write rf rd v in
            (State (pc + 1, rf, ram), Trace.RegMod (pc, inst, rd, v))

    | I_sl (rd, rs, imm) ->
            let v = (Rf.read rf rs) lsl imm in
            let () = Rf.write rf rd v in
            (State (pc + 1, rf, ram), Trace.RegMod (pc, inst, rd, v))

    | I_sr (rd, rs, imm) ->
            let v = (Rf.read rf rs) lsr imm in
            let () = Rf.write rf rd v in
            (State (pc + 1, rf, ram), Trace.RegMod (pc, inst, rd, v))

    | I_beq (rs1, rs2, imm) ->
            let imm_sext = M.sign_extend imm 6 in
            let st' =
                if Rf.read rf rs1 = Rf.read rf rs2
                then State (pc + imm_sext, rf, ram)
                else State (pc + 1, rf, ram) in
            (st', Trace.NoTrace (pc, inst))

    | I_blt (rs1, rs2, imm) ->
            let v1 = M.sign_extend (Rf.read rf rs1) 32 in
            let v2 = M.sign_extend (Rf.read rf rs2) 32 in
            let imm_sext = M.sign_extend imm 6 in
            let st' =
                if v1 < v2
                then State (pc + imm_sext, rf, ram)
                else State (pc + 1, rf, ram) in
            (st', Trace.NoTrace (pc, inst))

    | I_jalr (rd, rs, imm) ->
            let ret = pc + 1 in
            let imm_sext = M.sign_extend imm 6 in
            let target = (Rf.read rf rs) + imm_sext in
            let () = Rf.write rf rd ret in
            (State (target, rf, ram), Trace.RegMod (pc, inst, rd, ret))

    | I_apc (rd, imm) ->
            let v = (Rf.read rf rd) + pc + (M.sign_extend imm 11) in
            let () = Rf.write rf rd v in
            (State (pc + 1, rf, ram), Trace.RegMod (pc, inst, rd, v))

    | I_j imm ->
            let imm_sext = M.sign_extend imm 16 in
            (State (pc + imm_sext, rf, ram), Trace.NoTrace (pc, inst))


let step_prog_wt (State (pc, rf, ram) as st) prog =
    match List.nth_opt prog pc with
    | None -> None
    | Some inst ->
            Some (step_inst_wt st inst)

(* tcs is the traces list. *)
let rec execute_at_pc_wt ?(max_steps = None) (State (pc, rf, ram) as st) prog tcs =
    match max_steps with
    | None ->
            begin
            match List.nth_opt prog pc with
            | None -> (st, List.rev tcs)
            | Some inst ->
                    let (st', tc) = step_inst_wt st inst in
                    execute_at_pc_wt st' prog (tc :: tcs)
            end
    | Some n ->
            if n <= 0 then (st, List.rev tcs) else
            match List.nth_opt prog pc with
            | None -> (st, List.rev tcs)
            | Some inst ->
                    let (st', tc) = step_inst_wt st inst in
                    execute_at_pc_wt ~max_steps:(Some (n - 1)) st' prog (tc :: tcs)


let execute_prog_wt ?(max_steps = None) prog =
    let init_st = create () in
    execute_at_pc_wt ~max_steps:max_steps init_st prog []

let execute_prog_on_ram_wt ?(max_steps = None) ram prog =
    let init_st = State (0, Rf.create (), ram) in
    execute_at_pc_wt ~max_steps:max_steps init_st prog []


(* Executing and printing the trace. *)

let rec execute_at_pc_pt ?(max_steps = None) (State (pc, rf, ram) as st) prog =
    match max_steps with
    | None ->
            begin
            match List.nth_opt prog pc with
            | None ->
                    let () = print_endline "\nHALT\n" in st
            | Some inst ->
                    let (st', tc) = step_inst_wt st inst in
                    let () = print_endline (Trace.to_string tc) in
                    execute_at_pc_pt st' prog
            end
    | Some n ->
            if n <= 0 then st else
            match List.nth_opt prog pc with
            | None ->
                    let () = print_endline "\nHALT\n" in st
            | Some inst ->
                    let (st', tc) = step_inst_wt st inst in
                    let () = print_endline (Trace.to_string tc) in
                    execute_at_pc_pt ~max_steps:(Some (n - 1)) st' prog


let execute_prog_pt ?(max_steps = None) prog =
    let init_st = create () in
    execute_at_pc_pt ~max_steps:max_steps init_st prog

let execute_prog_on_ram_pt ?(max_steps = None) ram prog =
    let init_st = State (0, Rf.create (), ram) in
    execute_at_pc_pt ~max_steps:max_steps init_st prog

