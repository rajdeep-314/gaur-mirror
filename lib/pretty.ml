open Ast
open Bits

let rec pp_typ = function
  | TBits n ->
    Printf.sprintf "bits %i" n

  | TRecord r ->
    let pp_kv (f, t) = Printf.sprintf "%s: %s" f (pp_typ t) in
    let ft_list = StrMap.to_list r in
    let ft_list_pp = List.map pp_kv ft_list in
    let body = String.concat ", " ft_list_pp in
    Printf.sprintf "{%s}" body

    (* let pp_kv f t = Printf.sprintf "%s, %s" f (pp_typ t) in
    StrMap.fold (fun f t acc -> acc ^ (pp_kv f t)) r "" *)

  | TChip (t_in, t_out) ->
    Printf.sprintf "(%s) -> (%s)" (pp_typ t_in) (pp_typ t_out)

let rec pp_expr = function
  | Constant b -> 
    Printf.sprintf "%d" (bits_to_int b)

  | Instance (chip, expr) ->
    Printf.sprintf "%s %s" chip (pp_expr expr.expr_desc)

  | Splice (expr, start_index, end_index) ->
    Printf.sprintf "%s[%d:%d]" (pp_expr expr.expr_desc) start_index end_index

  | Concat (expr1, expr2) ->
    Printf.sprintf "%s ++ %s" (pp_expr expr1.expr_desc) (pp_expr expr2.expr_desc)

  | Record r ->
    let pp_kv (f, e) = Printf.sprintf "%s: %s" f (pp_expr e.expr_desc) in
    let ft_list = StrMap.to_list r in
    let ft_list_pp = List.map pp_kv ft_list in
    let body = String.concat ", " ft_list_pp in
    Printf.sprintf "{%s}" body

  | Access var ->
    Printf.sprintf "%s" var

  | Member (expr, field) ->
    Printf.sprintf "%s.%s" (pp_expr expr.expr_desc) field
