type addr = int

type t =
    | Help
    | SetTrace of bool
    | GetTrace
    | Reg of int
    | Regs
    | Print_inst
    | Print_prog
    | Mem of addr * int
    | Step
    | Steps of int
    | Quit


val to_string : t -> string
val p : t -> unit

