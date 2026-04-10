module Env         = Env
module Errors      = Errors
module Init        = Init
module Typechecker = Typechecker

(** [typecheck s] typechecks a parsed program [prog]. *)
let typecheck prog =
  let env = Init.v prog in
  Typechecker.tc_prog env prog
