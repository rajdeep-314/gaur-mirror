type addr = int

type t =
    | Help
    | SetTrace of bool
    | Reg of int
    | Regs
    | Print_prog
    | Mem of addr * int
    | Step
    | Steps of int
    | Quit


val to_string : t -> string
val p : t -> unit

