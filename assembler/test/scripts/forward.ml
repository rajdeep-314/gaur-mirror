(** forward.ml *)

(**
Generates test cases for forward branches and jumps.

Arguments:
    1. Instruction ([inst]): beq or blt or j
    2. Offset ([n]): a natural number >= 0

Prints to stdout an assembly program with instruction [inst], with offset [+n].
*)

let () =
    if Array.length Sys.argv <> 3 then
        begin
        prerr_endline "Usage: <beq|blt|j> <offset>" ;
        exit 1 ;
        end

let inst = Sys.argv.(1)
let offset =
    match int_of_string_opt Sys.argv.(2) with
    | Some n ->
        if n <= 0 then
            begin
            prerr_endline "Offset must be greater than 0" ;
            exit 2
            end
        else n
    | None   ->
        prerr_endline "Offset must be an integer" ;
        exit 3

let inst_line =
    match inst with
    | "beq" | "blt" -> inst ^ " r0 r0 target"
    | "j"           -> "j target"
    | _             ->
        prerr_endline "Invalid instruction" ;
        exit 4

let spin_list = List.init (offset - 1) (Fun.const "spin")

let prog = [inst_line] @ spin_list @ ["target:"]

let () =
    print_endline (String.concat "\n" prog)

