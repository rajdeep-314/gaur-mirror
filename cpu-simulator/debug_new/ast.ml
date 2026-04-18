(*
    ast.ml

    AST for the debug mode.
*)


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


let to_string = function
    | Help -> "help"
    | SetTrace true -> "set trace on"
    | SetTrace false -> "set trace off"
    | GetTrace -> "get trace"
    | Reg i -> "reg r" ^ string_of_int i
    | Regs -> "regs"
    | Print_inst -> "print inst"
    | Print_prog -> "print prog"
    | Mem (addr, count) -> "mem " ^ string_of_int addr ^ " " ^ string_of_int count
    | Step -> "step"
    | Steps n -> "steps " ^ string_of_int n
    | Quit -> "quit"

let print i = print_endline (to_string i)

