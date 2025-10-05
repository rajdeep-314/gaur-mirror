module Sast = Syntax.Ast
module Trace = Cpusim.Trace
module Ram = Cpusim.Ram
module Rf = Cpusim.Regfile
module Proc = Cpusim.Proc


let help_string =
    "Commands:

    help                    :   display help
    set trace [on | off]    :   toggle trace
    reg x                   :   print register rx
    regs                    :   print the entire register file
    print prog              :   print the loaded program
    mem addr count          :   print count-many RAM cells, starting from address addr
    step                    :   step through the program (one instruction)
    steps n                 :   step through the program (n instructions)
    quit                    :   exit the program"



let step prog proc trace =
    if trace then
        match Proc.step_prog_wt proc prog with
        | None ->
                let () = print_endline "PC out of bounds. Halting execution." in
                None
        | Some (proc', tr) -> 
                let () = print_endline (Trace.to_string tr) in
                Some proc'
    else
        match Proc.step_prog proc prog with
        | None ->
                let () = print_endline "PC out of bounds. Halting execution." in
                None
        | Some proc' -> 
                Some proc'


let rec steps prog proc_opt trace n =
    if n <= 0 then proc_opt
    else
    match proc_opt with
    | None -> None
    | Some proc ->
            steps prog (step prog proc trace) trace (n - 1)



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
    | Reg i ->
            let () = Rf.print_reg (Proc.get_regfile proc) (Sast.Reg.of_int i) in
            let () = print_endline "" in
            repl prog proc trace
    | Regs ->
            let () = Rf.print (Proc.get_regfile proc) in
            repl prog proc trace
    | Print_prog ->
            let () = Sast.print_prog prog in
            repl prog proc trace
    | Mem (addr, count) ->
            let count = if count <= 0 then 1 else count in
            let () = Ram.print_in_range (Proc.get_ram proc) addr (addr + count - 1) in
            repl prog proc trace
    | Step ->
            begin
            match step prog proc trace with
            | None -> ()
            | Some proc' -> repl prog proc' trace
            end
    | Steps n ->
            begin
            match steps prog (Some proc) trace n with
            | None -> ()
            | Some proc' -> repl prog proc' trace
            end
    | Quit ->
            print_endline "Exiting.\n"

