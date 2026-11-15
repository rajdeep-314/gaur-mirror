open Hdl_simulator
open Parse
open Analyze
open Typecheck
open Hdlutils

let usage_txt =
  Printf.sprintf "Usage: %s <file1.ghdl> [ <file2.ghdl> ... ]" Sys.argv.(0)
;;

let () =
  if Array.length Sys.argv < 2 then begin
    prerr_endline usage_txt;
    exit Exit.usage
  end
;;

let () =
  let files = List.tl (Array.to_list Sys.argv) in
  files
  |> Io.open_in_many
  |> parse_ic_many
  |> Toposort.toposort
  |> Typecheck.typecheck
  |> Causality.analyse
  |> ignore
;;
