module Env         = Env
module Errors      = Errors
module Init        = Init
module Typechecker = Typechecker

open Hdlutils
open Errors
open Typechecker

(** [typecheck s] typechecks a parsed program [prog]. *)
let typecheck prog =
  let env = Init.v prog in
  try tc_prog env prog
  with TypeError (loc, err) ->
    Loc.errorl loc;
    Pertty.pp Fmt.stderr err;
    exit Exit.typeerror
