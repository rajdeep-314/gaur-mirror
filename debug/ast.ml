type addr = int

type t =
    | Help
    | SetTrace of bool
    | GetTrace
    | Reg of int
    | Regs
    | Print_prog
    | Mem of addr * int
    | Step
    | Steps of int
    | Quit


(* Remove later. *)
let to_string = function
    | Help -> "help"
    | SetTrace true -> "set trace on"
    | SetTrace false -> "set trace off"
    | GetTrace -> "get trace"
    | Reg i -> "reg r" ^ string_of_int i
    | Regs -> "regs"
    | Print_prog -> "print prog"
    | Mem (addr, count) -> "mem " ^ string_of_int addr ^ " " ^ string_of_int count
    | Step -> "step"
    | Steps n -> "steps " ^ string_of_int n
    | Quit -> "quit"


let p s = print_endline (to_string s)

