open Lib.Parse
open Lib.Typecheck
open Lib.Hdlutils

let usage_txt =
  Printf.sprintf "Usage: %s <file.phdl>" Sys.argv.(0)

let () = Io.usage usage_txt

let () =
  let fname = Sys.argv.(1) in
  In_channel.with_open_text fname (parse_ic fname)
  |> typecheck
  |> ignore
