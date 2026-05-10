module E = Expressions
module F = Functions
module M = Modules
module N = Name
module P = Patterns
module T = Types

open Parse
open Hdlutils

module StrMap = Map.Make(String)

let rec typ_of (typ : Types.t) =
  match typ with
  | TBits   n  -> F.tbits n
  | TRecord tr -> typof_trec tr
  | TChip   _  -> failwith "TODO"

and typof_trec tr =
  tr
  |> StrMap.map (fun t -> typ_of t)
  |> StrMap.to_list
  |> List.map (fun (s, e) -> E.etuple [E.estring s; e])
  |> F.trecord_of_list

let typ_of_fun (name : string) (typ : Types.t) : E.let_bind =
  E.blet (P.pid name) (typ_of typ)

let typ_of_input (ci : 'a Ast.chip_info) =
  typ_of_fun N.fun_typ_of_input (ci.data.input.typ)

let typ_of_output (ci : 'a Ast.chip_info) =
  typ_of_fun N.fun_typ_of_output (ci.data.output.typ)
