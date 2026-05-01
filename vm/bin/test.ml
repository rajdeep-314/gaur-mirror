(*
    test.ml

    VM testing.
*)


module UF = Utils.Files
module VE = Vm.Emulator
module E  = Vm.Syntax.Parsing.Err
module Er = VE.Err
module I  = VE.State.Inspect
module P  = VE.State.PP


let argl = Array.to_list Sys.argv
let ps =
    match argl with
    | [] -> []
    | _ :: xs -> xs

let p =
    try
        List.map Vm.Syntax.Parsing.Vm.parse_from_path ps
    with
    | Vm.Syntax.Parsing.Err.LexingErrorWI _ as e ->
        prerr_endline (Printexc.to_string e) ;
        exit 1
    | Vm.Syntax.Parsing.Err.ParsingErrorWI _ as e ->
        prerr_endline (Printexc.to_string e) ;
        exit 2


let () =
    try
    let ip = VE.Vm2ir.translate_progs "main" p in
        try
            let s, n = VE.Emulate.e_prog_till_termination ip in
                Printf.printf "Execution terminated with %d\n%s\n"
                    n (P.screen s);
        with
        | Er.ExecutionError e -> prerr_endline (Er.Execution.to_string e)
    with
    | Er.SemanticError e  -> prerr_endline (Er.Semantic.to_string e)

