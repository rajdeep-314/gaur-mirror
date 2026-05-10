module Errors   = Errors
module Var      = Var
module Analyser = Analyser

open Errors
open Hdlutils

let analyse p =
  Analyser.analyse_prog (List.tl p);
  p
