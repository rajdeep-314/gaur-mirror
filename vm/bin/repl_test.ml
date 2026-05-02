(*
    repl_test.ml

    Testing the REPL.
*)


module VE = Vm.Emulator
module R  = VE.Repl
module UF = Utils.Files
module Er = VE.Err


let argl = Array.to_list Sys.argv
let ps = List.drop 1 argl

let ps, ps_scs =
    try
        List.split
        (List.map Vm.Syntax.Parsing.Vm.parse_from_path ps)
    with
    | Vm.Syntax.Parsing.Err.LexingErrorWI _
    | Vm.Syntax.Parsing.Err.ParsingErrorWI _ as e ->
        prerr_endline (Printexc.to_string e) ;
        exit 1
        | _ -> prerr_endline "got sum" ; exit 2


let ip =
    try
        VE.Vm2ir.translate_progs "main" ps
    with
    | e ->
        prerr_endline (Printexc.to_string e) ;
        exit 2

let () =
    try R.main_loop ip with
    | e ->
        prerr_endline (Printexc.to_string e) ;
        exit 2

