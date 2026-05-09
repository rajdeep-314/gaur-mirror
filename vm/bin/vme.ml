(*
    vme.ml

    The top-level script for the VM emulator.
*)

module VE = Vm.Emulator
module Er = VE.Err
module P  = VE.State.PP
module R  = VE.Repl
module STbl = Hashtbl.Make (String)

type src_tbl = string STbl.t

(* Configuration references with defaults *)
let verbose = ref false
let debug_mode = ref false
let show_screen = ref true
let show_rv = ref true
let files = ref []

let usage_msg = "Usage: vme [options] <file1.vm> <file2.vm> ..."

let speclist = [
    ("-v", Arg.Set verbose, " Verbose execution errors");
    ("--verbose", Arg.Set verbose, " Verbose execution errors");
    ("-nv", Arg.Clear verbose, " Do not display verbose execution errors");
    ("--no-verbose", Arg.Clear verbose, " Do not display verbose execution errors");

    ("-g", Arg.Set debug_mode, " REPL mode (debug)");
    ("--debug", Arg.Set debug_mode, " REPL mode (debug)");

    ("-s", Arg.Set show_screen, " Display final screen");
    ("--screen", Arg.Set show_screen, " Display final screen");
    ("-ns", Arg.Clear show_screen, " Do not display final screen");
    ("--no-screen", Arg.Clear show_screen, " Do not display final screen");

    ("-rv", Arg.Set show_rv, " Display final return value");
    ("--return-val", Arg.Set show_rv, " Display final return value");
    ("-nrv", Arg.Clear show_rv, " Do not display final return value");
    ("--no-return-val", Arg.Clear show_rv, " Do not display final return value");
]

let anon_fun arg = files := arg :: !files

let () =
    (* Parse command line *)
    Arg.parse speclist anon_fun usage_msg;

    (* Check if any files were provided *)
    if !files = [] then (
        prerr_endline "Error: No input files provided.";
        Arg.usage speclist usage_msg;
        exit 1
    );

    (* Parse all provided files (reversing list because Arg.parse prepends) *)
    let ps, ps_scs =
        try
            List.split (List.map Vm.Syntax.Parsing.Vm.parse_from_path (List.rev !files))
        with
        | Vm.Syntax.Parsing.Err.LexingErrorWI _ as e ->
            prerr_endline (Printexc.to_string e); exit 1
        | Vm.Syntax.Parsing.Err.ParsingErrorWI _ as e ->
            prerr_endline (Printexc.to_string e); exit 1
    in

    let src_tbl : src_tbl = STbl.of_seq (List.to_seq ps_scs) in

    try
        (* Translate to IR *)
        let ip = VE.Vm2ir.translate_progs "main" ps in

        if !debug_mode then
            (* REPL Mode *)
            R.main_loop src_tbl ip
        else
            (* Normal Execution Mode *)
            try
                let s, n = VE.Emulate.e_prog_till_termination ip in

                (* Handle Return Value output *)
                if !show_rv then (
                    match n with
                    | None -> print_endline "Execution terminated with no return value."
                    | Some v -> Printf.printf "Execution terminated with %d\n" v
                );

                (* Handle Screen output *)
                if !show_screen then (
                    let screen = P.screen s in
                    if screen <> "" then print_endline screen
                )
            with
            | Er.ExecutionError (a , e) ->
                let err_msg =
                    if !verbose then Er.pp_execution_error_verbose src_tbl (a, e)
                    else Er.pp_execution_error src_tbl (a, e)
                in
                prerr_endline err_msg;
                exit 2
    with
    | Er.SemanticError (a, e) ->
        prerr_endline (Er.pp_semantic_error src_tbl (a, e));
        exit 3