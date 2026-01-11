(*
    main.ml

    The top-level script of the simulator.
*)


(* Module aliases. *)
module Parser = Assembler.Parse
module Ast = Syntax.Ast
module Ram = Cpusim.Ram
module Rf = Cpusim.Regfile
module Proc = Cpusim.Proc

let path = ref ""               (* Input file path. *)
let ram_path = ref ""           (* Initial RAM image path. *)
let trace = ref false           (* Traces on? *)
let final_state = ref true      (* Final state printing on? *)
let max_steps = ref 2000        (* Max instructions to execute. *)
let debug_mode = ref false      (* Enter the debug mode? *)


let speclist = [
    ("--trace", Arg.Set trace, "Set trace to ON.");
    ("-t", Arg.Set trace, "Set trace to ON (short).");
    ("--no-trace", Arg.Clear trace, "Set trace to OFF.");
    ("-nt", Arg.Clear trace, "Set trace to OFF (short).");

    ("--final-state", Arg.Set final_state, "Set displaying final state to ON.");
    ("-fs", Arg.Set final_state, "Set displaying final state to ON (short).");
    ("--no-final-state", Arg.Clear final_state, "Set displaying final state to OFF.");
    ("-nfs", Arg.Clear final_state, "Set displaying final state to OFF (short).");

    ("--max-steps", Arg.Set_int max_steps, "Set the max number of execution states.");
    ("-ms", Arg.Set_int max_steps, "Set the max number of execution states (short).");
    ("--ram", Arg.Set_string ram_path, "Specify the initial RAM file's path.");
    ("-r", Arg.Set_string ram_path, "Specify the initial RAM file's path (short).");

    ("--debug", Arg.Set debug_mode, "Run the simulator in debug mode.");
    ("-g", Arg.Set debug_mode, "Run the simulator in debug mode (short).")
]

let usage_msg = "Usage: cpusim [options] <file_path>."

let anon_fun arg =
    if !path = "" then
        path := arg
    else
        begin
        print_endline "Error: Too many positional arguments (only one allowed).";
        Arg.usage speclist usage_msg;
        exit 1;
        end


let () =
    Arg.parse speclist anon_fun usage_msg

let init_ram =
    if !ram_path == "" then
        Ram.create ()
    else
        Bram.Parser.parse_file !ram_path


let () =
    if !path == "" then
        begin
        print_endline "Error: Input file path not passed as argument.";
        Arg.usage speclist usage_msg;
        exit 2;
        end

let prog_ast = Parser.parse_isa_prog_from_path !path


let () =
    (* If invoked with --debug or -g. *)
    if !debug_mode then
        Debug.Repl.repl prog_ast (Proc.State (0, Rf.create (), init_ram)) !trace
    else
    let proc_final_state =
        if !trace then
            Proc.execute_prog_on_ram_pt ~max_steps:(Some !max_steps) init_ram prog_ast
        else
            Proc.execute_prog_on_ram ~max_steps:(Some !max_steps) init_ram prog_ast
    in
    (* If not involed with --final-state or -fs. *)
    if !final_state then
        begin
        print_endline "\n\nFinal state\n----------------\n";
        Printf.printf "PC:\t%04x\n" (Proc.get_pc proc_final_state);
        print_endline "\nRegister file:";
        Rf.print (Proc.get_regfile proc_final_state);
        print_endline "\nRAM (non-zero values):";
        Ram.print_all_nz (Proc.get_ram proc_final_state);
        end

