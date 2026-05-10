module E = Expressions
module F = Functions
module M = Modules
module N = Name
module P = Patterns
module T = Types

open Parse
open Hdlutils

module StrMap = Map.Make(String)

let rec default_expr (typ : Types.t) : E.expr =
  match typ with
  | TBits   n  -> E.earray  (List.init n (fun _ -> E.efalse))
  | TRecord tr -> E.eobject (StrMap.map default_expr tr)
  | TChip   _  -> failwith "TODO"

let default_tin (ci : 'a Ast.chip_info) : E.expr =
  default_expr ci.data.input.typ
