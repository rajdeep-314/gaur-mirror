(*
    vme.ml

    The top-level script for the VM emulator.
*)


module VE = Vm.Emulator
module Er = VE.Err
module P  = VE.State.PP
module R  = VE.Repl
module S  = Vm.Syntax.Emulator
module STbl = Hashtbl.Make (String)

type src_tbl = string STbl.t


(* Configuration references with defaults *)
let verbose = ref false             (* Verbose execution errors. *)
let debug_mode = ref false          (* REPL for debugging. *)
let show_screen = ref true          (* Show final screen. *)
let show_rv = ref true              (* Show return value. *)
let files = ref []                  (* List of paths. *)
let help = ref false                (* Asking for help? *)


let usage_msg = "Usage: vme [options] <file1.vm> <file2.vm> ..."

let speclist = [
    ("-v", Arg.Set verbose, "\n\tVerbose execution errors");
    ("--verbose", Arg.Set verbose, "\n\tVerbose execution errors");
    ("-nv", Arg.Clear verbose, "\n\tDo not display verbose execution errors");
    ("--no-verbose", Arg.Clear verbose,
        "\n\tDo not display verbose execution errors");

    ("-g", Arg.Set debug_mode, "\n\tREPL mode (debug)");
    ("--debug", Arg.Set debug_mode, "\n\tREPL mode (debug)");

    ("-s", Arg.Set show_screen, "\n\tDisplay final screen");
    ("--screen", Arg.Set show_screen, "\n\tDisplay final screen");
    ("-ns", Arg.Clear show_screen, "\n\tDo not display final screen");
    ("--no-screen", Arg.Clear show_screen, "\n\tDo not display final screen");

    ("-rv", Arg.Set show_rv, "\n\tDisplay final return value");
    ("--return-val", Arg.Set show_rv, "\n\tDisplay final return value");
    ("-nrv", Arg.Clear show_rv, "\n\tDo not display final return value");
    ("--no-return-val", Arg.Clear show_rv,
        "\n\tDo not display final return value");

    ("--help", Arg.Set help, "\n\tDisplay this list of options");
    ("-help", Arg.Set help, "\n\tDisplay this list of options");
    ("-h", Arg.Set help, "\n\tShort for --help / -help" )
]

let anon_fun arg = files := arg :: !files

let () =
    (* Parse command line *)
    Arg.parse speclist anon_fun usage_msg;

    if !help then
        begin
        Arg.usage speclist usage_msg ;
        exit 0
        end ;

    (* Parse all provided files (reversing list because Arg.parse prepends) *)
    let ps, ps_scs =
        try
            List.split (List.map
                S.Parsing.Driver.parse_from_path (List.rev !files))
        with
        | S.Parsing.Err.LexingErrorWI _ as e ->
            prerr_endline (Printexc.to_string e); exit 1
        | S.Parsing.Err.ParsingErrorWI _ as e ->
            prerr_endline (Printexc.to_string e); exit 1
    in

    let src_tbl : src_tbl = STbl.of_seq (List.to_seq ps_scs) in
    try
        (* Translate to IR *)
        let ip = VE.Vm2ir.translate_progs "main" ps in

        (* REPL Mode *)
        if !debug_mode then
            R.main_loop src_tbl ip
        (* Normal Execution Mode *)
        else
            try
                let s, n = VE.Emulate.e_prog_till_termination ip in

                (* Handle return value. *)
                if !show_rv then
                    begin match n with
                    | None -> print_endline
                        "Execution terminated with no return value."
                    | Some v -> Printf.printf
                        "Execution terminated with %d\n" v end ;

                (* Handle Screen output *)
                if !show_screen then
                    begin
                    let screen = P.screen s in
                    if screen <> "" then print_endline screen
                    end
            with
            | Er.ExecutionError (a , e) ->
                let err_msg =
                    if !verbose then
                        Er.pp_execution_error_verbose src_tbl (a, e)
                    else
                        Er.pp_execution_error src_tbl (a, e)
                in
                prerr_endline err_msg;
                exit 3
    with
    | Er.SemanticError (a, e) ->
        prerr_endline (Er.pp_semantic_error src_tbl (a, e));
        exit 2

