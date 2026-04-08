open Lib.Parse
open Lib.Hdlutils

let usage_txt =
  Printf.sprintf "Usage: %s <file.phdl>" Sys.argv.(0)

let () = Io.usage usage_txt

let () = In_channel.with_open_text Sys.argv.(1) parse_ic |> ignore
