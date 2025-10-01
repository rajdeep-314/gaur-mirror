module Parser = Cpusim.Parser
module Ast = Cpusim.Ast
module Ram = Cpusim.Ram
module Rf = Cpusim.Regfile
module Proc = Cpusim.Proc


(* Parameters. *)
let path = ref ""
let ram_path = ref ""
let trace = ref false
let final_state = ref true
let max_steps = ref 5000

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
    ("-r", Arg.Set_string ram_path, "Specify the initial RAM file's path (short).")
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


(* Placeholder functions: BEGIN *)
let placeholder_func_1 _ = Ram.create ()
(* Placeholder functions: END *)

let () =
    Arg.parse speclist anon_fun usage_msg

let init_ram =
    if !ram_path == "" then
        Ram.create ()
    else
        placeholder_func_1 !ram_path


let () =
    if !path == "" then
        begin
        print_endline "Error: File path not passed as argument.";
        Arg.usage speclist usage_msg;
        exit 2;
        end

let prog =
    Parser.parse_from_path !path

let () =
    let proc_final_state =
        if !trace then
            Proc.execute_prog_on_ram_pt init_ram prog
        else
            Proc.execute_prog_on_ram init_ram prog
    in
    if !final_state then
        begin
        print_endline "\n\nFinal state\n----------------\n";
        Printf.printf "PC:\t%04x\n" (Proc.get_pc proc_final_state);
        print_endline "\nRegister file:";
        Rf.print (Proc.get_regfile proc_final_state);
        print_endline "\nRAM (non-zero values):";
        Ram.print_all_nz (Proc.get_ram proc_final_state);
        end

