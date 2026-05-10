module Toposorter = Toposorter
module Errors     = Errors

open Errors
open Hdlutils

(** [toposort prog] sorts prog in topological order of chip
    chip dependencies. It raises error if any cycles are detected.
*)
let toposort prog =
  try Toposorter.toposort prog
  with CycleError err ->
    Pretty.pp Fmt.stderr err;
    exit Exit.cycleerror
