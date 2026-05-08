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

type cmd =
  | Input   of expr_info
  | Output
  | Clock
  | Quit
