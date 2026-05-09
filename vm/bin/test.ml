(*
    test.ml

    VM testing.
*)


module UF = Utils.Files
module VE = Vm.Emulator
module E  = Vm.Syntax.Vm.Parsing.Err
module Er = VE.Err
module I  = VE.State.Inspect
module P  = VE.State.PP

module STbl = Hashtbl.Make (String)
type src_tbl = string STbl.t


let argl = Array.to_list Sys.argv
let ps =
    match argl with
    | [] -> []
    | _ :: xs -> xs

let ps, ps_scs =
    try
        List.split
        (List.map Vm.Syntax.Vm.Parsing.Driver.parse_from_path ps)
    with
    | Vm.Syntax.Vm.Parsing.Err.LexingErrorWI _ as e ->
        prerr_endline (Printexc.to_string e) ;
        exit 1
    | Vm.Syntax.Vm.Parsing.Err.ParsingErrorWI _ as e ->
        prerr_endline (Printexc.to_string e) ;
        exit 1


let src_tbl : src_tbl =
    STbl.of_seq (List.to_seq ps_scs)


let () =
    try
    let ip = VE.Vm2ir.translate_progs "main" ps in
        try
            let s, n = VE.Emulate.e_prog_till_termination ip in
            let screen = P.screen s in
            match n with
            | None -> Printf.printf
                "Execution terminated with no return value.\n%s\n" screen
            | Some n -> Printf.printf
                "Execution terminated with %d\n%s\n" n screen;
        with
        | Er.ExecutionError (a, e) ->
            prerr_endline (Er.pp_execution_error_verbose src_tbl (a, e)) ;
            exit 2
    with
    | Er.SemanticError (a, e) ->
        prerr_endline (Er.pp_semantic_error src_tbl (a, e)) ;
        exit 3

