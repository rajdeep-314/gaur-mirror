open Parse
open Select
open Files
open Runrepl

(** [compile prog] compiles a program [prog] carrying no additional
    information.
*)
let compile p : file =
  let p = Rename.prog p in
  let module P = struct
    type t = unit

    let prog : t Ast.prog = p
  end
  in
  let module CP = Statetop.Make(P) () in
  List.flatten [
    [ fopen "Runtime" ]
  ; fmodule_list CP.v
  ; [ ffun (select_fun p) ]
  ; [ ffun select_fail_fun ]
  ; [ ffun run_repl_fun ]
  ; [ ffun run_bench_fun ]
  ; [ ffun run_fun ]
  ]
