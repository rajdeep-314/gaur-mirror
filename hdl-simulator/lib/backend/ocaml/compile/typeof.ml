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

let typeof_fun (name : string) (typ : Types.t) : E.let_bind =
  E.blet (P.pid name) (typeof typ)

let typeof_tin (ci : 'a Ast.chip_info) =
  typeof_fun N.fun_typeof_tin (ci.data.input.typ)

let typeof_tout (ci : 'a Ast.chip_info) =
  typeof_fun N.fun_typeof_tout (ci.data.output.typ)
