open Parse
open Typecheck.Typechecker
open Summary
open Errors

let is_nand id = id = Typecheck.Init.nand_name

let analyse_prog (prog : tc_info Ast.prog) : Summary.Env.env =
  List.fold_left
    (fun env (ci : tc_info Ast.chip_info) ->
       if is_nand ci.data.name then
         env
       else
         let summary = Dep_graph.build_and_check_chip env ci in
         Summary.Env.add summary env)
    Summary.Env.init
    prog
;;
