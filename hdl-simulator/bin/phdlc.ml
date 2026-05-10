open Lib
open Backend.Ocaml
open Hdlutils
open Analyze

(* Argument Parsing *)

let usage_msg =
  Io.make_usage_msg "chip1.phdl [ chip2.phdl ... ]"

let output_name = ref "a.ml"
let input_files = ref []

let add_file s = input_files := s :: !input_files

let spec_list = [
  ("-o", Arg.Set_string output_name, " Set output file name (without extenstion)")
]

let _ =
  Arg.parse spec_list add_file usage_msg;

  match !input_files with
  | [] ->
    prerr_endline (Arg.usage_string spec_list usage_msg);
    exit Exit.usage

  | files ->
    let files = List.rev files in
    let out = !output_name in
    let out_ml = out ^ ".ml" in
    let out_byte = out ^ ".byte" in
    let outx = Out_channel.open_text out_ml in
    let outf = Format.formatter_of_out_channel outx in

    let _ =
      files
      |> Io.open_in_many
      |> Parse.parse_ic_many
      |> Toposort.toposort
      |> Typecheck.typecheck
      |> Liveness.analyse
      |> Parse.Ast.Map.erase
      |> Compile.compile
      |> Files.Pretty.pp_file outf
    in

    Format.pp_print_flush outf ();
    Out_channel.flush outx;
    Out_channel.close outx;

    let cmd = Printf.sprintf "ocamlformat --inplace --enable-outside-detected-project %s" out_ml in
    let res = Sys.command cmd in
    if res <> 0
    then Printf.eprintf "Error: ocamlformat failed with exit code %d." res;

    let bin_path =
      Sys.executable_name
      |> Filename.dirname
    in

    let cwd = Sys.getcwd () in

    let runtime_path = Format.sprintf "\"%s/runtime\"" bin_path in
    let runtime_cma = "runtime.cma" in
    let cmd =
      Printf.sprintf
        "ocamlfind ocamlc -I %s -package fmt -linkpkg -o \"%s/%s\" %s \"%s/%s\""
        runtime_path
        cwd
        out_byte
        runtime_cma
        cwd
        out_ml
    in
    let res = Sys.command cmd in
    if res <> 0
    then Printf.eprintf "Error: ocamlc failed with exit code %d." res
