open Ast
type pc = int

type t =
    | State of pc * Regfile.t * Ram.t

let create () = State (0, Regfile.create (), Ram.create ())

let step (State (pc, rf, ram)) inst =
    match inst with
    | Lli (rd, imm) ->
            State (pc + 1, rf, ram)
    | Lui (rd, imm) ->
            State (pc + 1, rf, ram)
    | Ld (rd, rs, imm) ->
            State (pc + 1, rf, ram)
    | St (rs1, rs2, imm) ->
            State (pc + 1, rf, ram)
    | Add (rd, rs1, rs2) ->
            State (pc + 1, rf, ram)
    | Addi (rd, rs, imm) ->
            State (pc + 1, rf, ram)
    | Sub (rd, rs1, rs2) ->
            State (pc + 1, rf, ram)
    | And (rd, rs1, rs2) ->
            State (pc + 1, rf, ram)
    | Not (rd, rs) ->
            State (pc + 1, rf, ram)
    | Beq (rs1, rs2, imm) ->
            State (pc + 1, rf, ram)
    | Blt (rs1, rs2, imm) ->
            State (pc + 1, rf, ram)
    | Jalr (rd, rs, imm) ->
            State (pc + 1, rf, ram)
    | Apc (rd, imm) ->
            State (pc + 1, rf, ram)
    | J imm ->
            State (pc + 1, rf, ram)

