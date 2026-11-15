module Summary = Summary
module Errors = Errors
module Dep_graph = Dep_graph
module Analyser = Analyser

open Common.Error
open Hdlutils
open Errors
open Typecheck.Typechecker

let analyse_exn (prog : tc_info Parse.Ast.prog) : tc_info Parse.Ast.prog =
  let _ = Analyser.analyse_prog prog in
  prog
;;

let analyse_diag (prog : tc_info Parse.Ast.prog) : (tc_info Parse.Ast.prog, Diagnostics.t) result =
  try Ok (analyse_exn prog)
  with CausalityError error -> Error error
;;

open Common

let analyse_result ?lookup ?(resolver = Source.default_resolver) (prog : tc_info Parse.Ast.prog) : (tc_info Parse.Ast.prog, string) result =
  let resolver =
    match lookup with
    | Some l -> Source.of_string_lookup l
    | None -> resolver
  in
  try Ok (analyse_exn prog)
  with CausalityError error ->
    Error (Fmt.to_to_string (Diagnostics.pp_short_with resolver) error)
;;

let analyse (prog : tc_info Parse.Ast.prog) : tc_info Parse.Ast.prog =
  try analyse_exn prog
  with CausalityError error ->
    Diagnostics.pp_short Fmt.stderr error;
    exit Exit.cycleerror
;;
