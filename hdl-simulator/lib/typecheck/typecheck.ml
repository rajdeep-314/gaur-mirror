module Env         = Env
module Errors      = Errors
module Init        = Init
module Typechecker = Typechecker

open Common.Error
open Hdlutils
open Errors
open Typechecker
open Parse.Ast

(** [typecheck prog] typechecks a parsed program [prog]. All
    typechecking information is preserved.
*)
let typecheck prog =
  let prog' = Init.prog Loc.dummy prog in
  let env = Init.env prog' in
  try tc_prog env prog'
  with TypeError error ->
    Diagnostics.pp_short Fmt.stderr error;
    exit Exit.typeerror
