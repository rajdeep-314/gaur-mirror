type addr = int

type t =
    | Help
    | SetTrace of bool
    | Reg of int
    | Regs
    | Mem of addr * int
    | Step
    | Steps of int
    | Quit

