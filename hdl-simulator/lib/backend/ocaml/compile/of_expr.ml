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

let rec of_expr (typ : Types.t) (e : E.expr) =
  match typ with
  | TBits   n  -> F.(e ||> get_bv)
  | TRecord tr -> of_expr_trec tr e
  | TChip   _  -> failwith "TODO"

and of_expr_trec tr e =
  tr
  |> StrMap.to_list
  |> List.map (fun (s, t) -> (s, of_expr t F.(e ||>? (E.estring s))))
  |> StrMap.of_list
  |> E.eobject

let of_expr_fun (name : string) (typ : Types.t) (tout : T.typ option) =
  E.bfun
    name
    [ pe, None ]
    tout
    (of_expr typ ee)

let input_of_expr (ci : 'a Ast.chip_info) : E.let_bind =
  of_expr_fun
    N.fun_input_of_expr
    ci.data.input.typ
    (Some (T.tid N.id_input))

let output_of_expr (ci : 'a Ast.chip_info) : E.let_bind =
  of_expr_fun
    N.fun_output_of_expr
    ci.data.output.typ
    (Some (T.tid N.id_output))
