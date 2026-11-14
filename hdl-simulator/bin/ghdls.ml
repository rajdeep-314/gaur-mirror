open Hdl_simulator
open Hdlutils
open Analyze
open Backend.Interpreter

(* Argument Parsing *)

let usage_msg =
  Hdlutils.Io.make_usage_msg "chip1.ghdl [ chip2.ghdl ... ]"

let chip_name = ref "Nand"
let input_files = ref []

let add_file s = input_files := s :: !input_files

let spec_list = [
  ("-name", Arg.Set_string chip_name, " Set chip name to simulate")
]

let _ =
  Arg.parse spec_list add_file usage_msg;

  match !input_files with
  | [] ->
    prerr_endline (Arg.usage_string spec_list usage_msg);
    exit Exit.usage

  | files ->
    let files = List.rev files in
    let _ =
      files
      |> Hdlutils.Io.open_in_many
      |> Parse.parse_ic_many
      |> Toposort.toposort
      |> Typecheck.typecheck
      |> Causality.analyse
      |> Liveness.analyse
      |> Reduce.reduce_prog
      |> Repl.repl !chip_name
    in
    ()
