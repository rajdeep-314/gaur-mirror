module Env         = Env
module Errors      = Errors
module Init        = Init
module Typechecker = Typechecker

open Common
open Common.Error
open Hdlutils
open Errors
open Typechecker
open Parse.Ast

let typecheck_exn prog =
  let prog' = Init.prog Loc.dummy prog in
  let env = Init.env prog' in
  tc_prog env prog'

let typecheck_diag prog =
  try Ok (typecheck_exn prog)
  with TypeError error -> Error error

let typecheck_result ?lookup ?(resolver = Source.default_resolver) prog =
  let resolver =
    match lookup with
    | Some l -> Source.of_string_lookup l
    | None -> resolver
  in
  try Ok (typecheck_exn prog)
  with TypeError error ->
    Error (Fmt.to_to_string (Diagnostics.pp_short_with resolver) error)

(** [typecheck prog] typechecks a parsed program [prog]. All
    typechecking information is preserved.
*)
let typecheck prog =
  try typecheck_exn prog
  with TypeError error ->
    Diagnostics.pp_short Fmt.stderr error;
    exit Exit.typeerror
