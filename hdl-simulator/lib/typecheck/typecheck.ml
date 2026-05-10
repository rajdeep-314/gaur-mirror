module Env         = Env
module Errors      = Errors
module Init        = Init
module Typechecker = Typechecker

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
  with TypeError (loc, err) ->
    Loc.errorl loc;
    Pretty.pp Fmt.stderr err;
    exit Exit.typeerror

(** [typecheck_erase prog] typechecks a program [prog], and erases
    all typechecking information if it succeeds.
*)
let typecheck_erase prog =
  let erase = Map.prog_info (fun _ -> ()) in
  erase (typecheck prog)
