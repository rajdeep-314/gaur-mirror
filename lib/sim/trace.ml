(* CPU execution traces *)


module Rf = Regfile


type t =
    | RegMod of Ast.Reg.t * Ram.half
    | RamMod of Ram.addr * Ram.half
    | NoTrace


let to_string = function
    | RegMod (r, v) ->
            Printf.sprintf "%s\t%04x" (Ast.Reg.to_string r) v
    | RamMod (addr, v) ->
            Printf.sprintf "%04x\t%04x" addr v
    | NoTrace -> "-"


