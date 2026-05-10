open Lib
open Lib.Backend.Ocaml

let argc = Array.length Sys.argv
let () = if argc <= 1 then exit 1

let file = Sys.argv.(1)
let ic = In_channel.open_text file

let p =
  Parse.parse_ic file ic
  |> Typecheck.typecheck_erase
  |> Compile.compile
  |> Modules.Pretty.pp_decl_list Fmt.stdout
