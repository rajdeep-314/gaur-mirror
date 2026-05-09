open Lib
open Lib.Backend.Ocaml

let argc = Array.length Sys.argv
let () = if argc <= 1 then exit 1

let file = Sys.argv.(1)
let ic = In_channel.open_text file

let p =
  Parse.parse_ic file ic
  |> Typecheck.typecheck
  |> Compile.Rename.prog

module P = struct
  type t = Typecheck.Typechecker.tc_info
  let prog = p
end

module Q = Compile.Top.Make(P)()

let () = Fmt.list Modules.Pretty.pp_decl Fmt.stdout (List.tl Q.v)
