(**
    Conversion to runtime expressions
*)

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

let rec expr (typ : Types.t) (e : E.expr) =
  match typ with
  | TBits   _  -> F.ebits e
  | TRecord tr -> expr_erec tr e
  | TChip   _  -> failwith "TODO"

and expr_erec tr e =
  tr
  |> StrMap.to_list
  |> List.map (fun (s, t) -> E.etuple [E.estring s; expr t (E.eobjmem e s)])
  |> F.erecord_of_list

let to_expr (name : string) (typ : Types.t) (tout : T.typ option) : E.let_bind =
  E.bfun
    name
    [ pe, tout ]
    None
    (expr typ ee)

let expr_of_input (ci : 'a Ast.chip_info) =
  to_expr
    N.fun_expr_of_input
    (ci.data.input.typ)
    (Some (T.tid N.id_input))

let expr_of_output (ci : 'a Ast.chip_info) =
  to_expr
    N.fun_expr_of_output
    (ci.data.output.typ)
    (Some (T.tid N.id_output))
