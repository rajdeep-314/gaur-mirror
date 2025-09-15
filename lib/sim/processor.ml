(*
To do:
    - Make all this cleaner. Delegate work to helper functions.
    - Fetching the instruction itself in step? A wrapper function for that?
    - imm to int conversion functions for signed and unsigned immediates?
      Pass that as an argument? Can have default arguments.
*)

open Ast

module Rf = Regfile

type pc = int
type t =
    | State of pc * Regfile.t * Ram.t

let create () = State (0, Regfile.create (), Ram.create ())


let step (State (pc, rf, ram)) inst =
    match inst with
    | Lli (rd, imm) ->
            let data = Rf.read rf rd in
            let data' = (data land 0xff00) lor (Bit.imm8_to_int imm) in
            let () = Rf.write rf rd data' in
            State (pc + 1, rf, ram)
    | Lui (rd, imm) ->
            let data = Rf.read rf rd in
            let data' = (data land 0xff) lor ((Bit.imm8_to_int imm) lsl 8) in
            let () = Rf.write rf rd data' in
            State (pc + 1, rf, ram)
    | Ld (rd, rs, imm) ->
            (* To do *)
            State (pc + 1, rf, ram)
    | St (rs1, rs2, imm) ->
            (* To do *)
            State (pc + 1, rf, ram)
    | Add (rd, rs1, rs2) ->
            let rs1v = Rf.read rf rs1 in
            let rs2v = Rf.read rf rs2 in
            let () = Rf.write rf rd (rs1v + rs2v) in
            State (pc + 1, rf, ram)
    | Addi (rd, rs, imm) ->
            let rsv = Rf.read rf rs in
            let () = Rf.write rf rd (rsv + Bit.imm4_to_int imm) in
            State (pc + 1, rf, ram)
    | Sub (rd, rs1, rs2) ->
            let rs1v = Rf.read rf rs1 in
            let rs2v = Rf.read rf rs2 in
            let () = Rf.write rf rd (rs1v - rs2v) in
            State (pc + 1, rf, ram)
    | And (rd, rs1, rs2) ->
            let rs1v = Rf.read rf rs1 in
            let rs2v = Rf.read rf rs2 in
            let () = Rf.write rf rd (rs1v land rs2v) in
            State (pc + 1, rf, ram)
    | Not (rd, rs) ->
            let rsv = Rf.read rf rs in
            let () = Rf.write rf rd (lnot rsv) in
            State (pc + 1, rf, ram)
    | Beq (rs1, rs2, imm) ->
            (* To do *)
            State (pc + 1, rf, ram)
    | Blt (rs1, rs2, imm) ->
            (* To do *)
            State (pc + 1, rf, ram)
    | Jalr (rd, rs, imm) ->
            (* To do *)
            State (pc + 1, rf, ram)
    | Apc (rd, imm) ->
            let rdv = Rf.read rf rd in
            let () = Rf.write rf rd (rdv + pc + Bit.imm8_to_int imm) in
            State (pc + 1, rf, ram)
    | J imm ->
            State (pc + Bit.imm12_to_int imm, rf, ram)

