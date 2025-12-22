(*
    repl.ml

    The debug mode's REPL.
*)


(* Module aliases. *)
module Sast = Syntax.Ast
module Trace = Cpusim.Trace
module Ram = Cpusim.Ram
module Rf = Cpusim.Regfile
module Proc = Cpusim.Proc


let help_string =
    "Commands:

    help                    :   display help
    set trace [on | off]    :   toggle trace
    get trace               :   see if trace is ON or OFF
    reg x                   :   print register rx
    regs                    :   print the entire register file
    print inst              :   print the instruction that PC currently points to
    print prog              :   print the loaded program
    mem addr count          :   print count-many RAM cells, starting from address addr
    step                    :   step through the program (one instruction)
    steps n                 :   step through the program (n instructions)
    quit                    :   exit the program"


let step prog proc trace =
    if trace then
        match Proc.step_prog_wt proc prog with
        | None ->
                let () = print_endline "Error: PC out of bounds." in
                (proc, true)
        | Some (proc', tr) -> 
                let () = print_endline (Trace.to_string tr) in
                (proc', false)
    else
        match Proc.step_prog proc prog with
        | None ->
                let () = print_endline "Error: PC out of bounds." in
                (proc, true)
        | Some proc' ->
                (proc', false)


let rec steps prog proc trace n =
    if n <= 0 then proc
    else
        let (proc', out_of_bounds) = step prog proc trace in
        if out_of_bounds then proc'
        else steps prog proc' trace (n - 1)


let rec repl prog proc trace =
    let () = print_string "\ncpusim> " in
    match Parser.parse_line_opt (read_line ()) with
    | None ->
            let () = print_endline "Syntax error." in
            repl prog proc trace
    | Some cmd ->
    match cmd with
    | Help ->
            let () = print_endline help_string in
            repl prog proc trace 
    | SetTrace true ->
            let () = print_endline "Traces are now ON." in
            repl prog proc true
    | SetTrace false ->
            let () = print_endline "Traces are now OFF." in
            repl prog proc false
    | GetTrace ->
            let str = if trace then "ON" else "OFF" in
            let () = Printf.printf "Traces are %s.\n" str in
            repl prog proc trace
    | Reg i ->
            let () = Rf.print_reg (Proc.get_regfile proc) (Sast.Reg.of_int i) in
            let () = print_endline "" in
            repl prog proc trace
    | Regs ->
            let () = Rf.print (Proc.get_regfile proc) in
            repl prog proc trace
    | Print_inst ->
            let () = 
            match List.nth_opt prog (Proc.get_pc proc) with
            | None -> print_endline "Error: PC out of bounds, no instruction to display."
            | Some inst -> print_endline (Sast.to_string inst)
            in
            repl prog proc trace
    | Print_prog ->
            let () = Sast.print_prog_with_pc prog (Proc.get_pc proc) in
            repl prog proc trace
    | Mem (addr, count) ->
            let count = if count <= 0 then 1 else count in
            let () = Ram.print_in_range (Proc.get_ram proc) addr (addr + count - 1) in
            repl prog proc trace
    | Step ->
            let (proc', _) = step prog proc trace in
            repl prog proc' trace
    | Steps n ->
            repl prog (steps prog proc trace n) trace
    | Quit ->
            print_endline "Exiting.\n"

