module E = Expressions
module F = Functions
module M = Modules
module N = Name
module P = Patterns
module T = Types

open Parse
open Hdlutils

module StrMap = Map.Make(String)

let ee = E.eid "e"
let pe = P.pid "e"

let rec totype (typ : Types.t) (e : E.expr) =
  match typ with
  | TBits   n  -> F.(e ||> get_bv)
  | TRecord tr -> totype_trec tr e
  | TChip   _  -> failwith "TODO"

and totype_trec tr e =
  tr
  |> StrMap.to_list
  |> List.map (fun (s, t) -> (s, totype t F.(e ||>? (E.estring s))))
  |> StrMap.of_list
  |> E.eobject

let totype_fun (name : string) (typ : Types.t) (tout : T.typ option) =
  E.bfun
    name
    [ pe, None ]
    tout
    (totype typ ee)

let totype_tin (ci : 'a Ast.chip_info) : E.let_bind =
  totype_fun
    N.fun_input_of_expr
    ci.data.input.typ
    (Some (T.tid N.id_input))

let totype_tout (ci : 'a Ast.chip_info) : E.let_bind =
  totype_fun
    N.fun_output_of_expr
    ci.data.output.typ
    (Some (T.tid N.id_output))
