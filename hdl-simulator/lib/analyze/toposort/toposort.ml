module Toposorter = Toposorter
module Errors     = Errors

open Errors
open Hdlutils

let toposort_exn = Toposorter.toposort

let toposort_diag prog =
  try Ok (Toposorter.toposort prog)
  with CycleError err -> Error err

let toposort_result prog =
  try Ok (Toposorter.toposort prog)
  with CycleError err -> Error (Fmt.to_to_string Pretty.pp err)

(** [toposort prog] sorts prog in topological order of chip
    chip dependencies. It raises error if any cycles are detected.
*)
let toposort prog =
  try toposort_exn prog
  with CycleError err ->
    Pretty.pp Fmt.stderr err;
    exit Exit.cycleerror
