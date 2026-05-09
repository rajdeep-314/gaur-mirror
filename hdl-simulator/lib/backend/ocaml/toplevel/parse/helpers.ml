open Ast

(** [concat b₁ b₂] is concatenation of [b₁] and [b₂], [b₁b₂]. *)
let concat = Array.append
let ( ++ ) = concat

(** [splice b l r] is the bitvector [b] from index [l] to [r], both
    inclusive.
*)
let splice bv lb ub = Array.sub bv lb (ub - lb + 1)

(** [nand b₁ b₂] is [not (b₁ && b₂)]. *)
let nand b1 b2 = not (b1 && b2)

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

let ebits bv = Const bv

let erecord_of_list (el : (string * expr) list) =
  let er =
    el
    |> List.map (fun (s, e) -> (s, mk_expr_info e Loc.dummy))
    |> StrMap.of_list
  in
  Record er
