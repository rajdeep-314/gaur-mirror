module E = Expressions
module F = Functions
module M = Modules
module N = Name
module P = Patterns
module T = Types

open Parse
open Hdlutils

module StrMap = Map.Make(String)

let rec typeof (typ : Types.t) =
  match typ with
  | TBits   n  -> F.tbits n
  | TRecord tr -> typeof_trec tr
  | TChip   _  -> failwith "TODO"

and typeof_trec tr =
  tr
  |> StrMap.map (fun t -> typeof t)
  |> StrMap.to_list
  |> List.map (fun (s, e) -> E.etuple [E.estring s; e])
  |> F.trecord_of_list

let type_fun (name : string) (typ : Types.t) : E.let_bind =
  E.blet (P.pid name) (typeof typ)

let input_type (ci : 'a Ast.chip_info) =
  type_fun N.fun_input_type (ci.data.input.typ)

let output_type (ci : 'a Ast.chip_info) =
  type_fun N.fun_output_type (ci.data.output.typ)
