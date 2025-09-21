type t =
    | RegMod of Ast.Reg.t * Ram.half
    | RamMod of Ram.addr * Ram.half
    | NoTrace

val to_string : t -> string

