module E = Expressions
module F = Functions
module M = Modules
module N = Name
module P = Patterns
module T = Types

open Parse
open Hdlutils

module StrMap = Map.Make(String)

let v = "v"

let comma_sp ppf = Fmt.any ", " ppf

let format_of_tr_fld ppf ((s, _) : string * Types.t) =
  Fmt.pf ppf "%s = %s " s "%a"

let format_of_tr (tr : Types.t StrMap.t) : E.expr =
  Fmt.str "{ %a}"
    (Fmt.list ~sep:comma_sp format_of_tr_fld) (StrMap.to_list tr)
  |> E.eid

let rec pretty_typ (typ : Types.t) : E.expr =
  match typ with
  | TBits   n  -> F.pp_bv
  | TRecord tr ->
    let format_expr = format_of_tr tr in
    let args =
      tr
      |> StrMap.to_list
      |> List.map fmt_args
      |> List.flatten
    in
    E.efun
      [ P.pid "ppf", None
      ; P.pid v    , None
      ]
      None
      (F.fmt_pf_ppf (format_expr :: args))

  | TChip   _  -> failwith "TODO"

and fmt_args ((s, typ) : string * Types.t) =
  [ pretty_typ typ
  ; E.eobjmem (E.eid v) s
  ]

let pretty_fun (name : string) (typ : Types.t) : E.let_bind =
  E.blet (P.pid name) (pretty_typ typ)

(* let pretty (ci : 'a Ast.chip_info) =
  let chip = ci.data in
  let states_pretty =
    chip.states
      |> StrMap.to_list
      |> List.map (fun (s, t) -> pretty_fun s t)
  in
  [ pretty_fun chip.input.name  chip.input.typ
  ; pretty_fun chip.output.name chip.output.typ
  ] @ states_pretty
 *)

let pretty (ci : 'a Ast.chip_info) =
  let chip = ci.data in
  [ pretty_fun N.fun_pp_tin  chip.input.typ
  ; pretty_fun N.fun_pp_tout chip.output.typ
  ]
