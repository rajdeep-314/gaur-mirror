open Bits

module StrMap = Map.Make (String)

type expr =
  | Const  of bv
  | Record of expr_info StrMap.t

and expr_info =
  { expr : expr
  ; loc  : Loc.t
  }

let get_expr e = e.expr

let get_bv e =
  match e with
  | Const bv -> bv
  | Record _ -> failwith "Can not get bitvector from a record."

let get_record e f =
  match e with
  | Const  _  -> failwith "Can not get record from a bitvector."
  | Record er ->
    let m = StrMap.map get_expr er in
    let e = StrMap.find_opt f m in
    let fail () = failwith ("Key not found: " ^ f) in
    Option.value ~default:(fail ()) e

let ( |>? ) = get_record

let mk_expr_info e l =
  { expr=e
  ; loc=l
  }

let ebits bv = Const bv

let erecord_of_list (el : (string * expr) list) =
  let er =
    el
    |> List.map (fun (s, e) -> (s, mk_expr_info e Loc.dummy))
    |> StrMap.of_list
  in
  Record er

type cmd =
  | Input   of expr_info
  | Output
  | Clock
  | Quit


module Pretty = struct
  let pp_bit ppf b = Fmt.int ppf (Bool.to_int b)
  let pp_bv ppf bv = Fmt.pf ppf "@[<h>%a@]" Fmt.(array pp_bit) bv

  let rec pp_expr ppf expr =
    match expr with
    | Const  bv -> pp_bv ppf bv
    | Record er -> Fmt.(braces (list pp_field)) ppf (StrMap.to_list er)

  and pp_field ppf (f, e) =
    Fmt.pf ppf "%s = %a" f pp_expr e.expr

  let pp_cmd ppf cmd =
    match cmd with
    | Input ei -> Fmt.pf ppf "INPUT %a" pp_expr ei.expr
    | Output   -> Fmt.pf ppf "OUTPUT"
    | Clock    -> Fmt.pf ppf "CLOCK"
    | Quit     -> Fmt.pf ppf "QUIT"
end
