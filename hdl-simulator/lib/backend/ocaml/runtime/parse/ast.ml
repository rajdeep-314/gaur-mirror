module StrMap = Map.Make (String)

(** A singluar bit *)
type bit = bool

(** A bit vector *)
type bv  = bit array

type 'i expr =
  | Const  of bv
  | Record of 'i expr_info StrMap.t

and 'i expr_info =
  { expr : 'i expr
  ; info  : 'i
  }

let get_expr e = e.expr

let mk_expr_info e i =
  { expr=e
  ; info=i
  }

type 'i cmd =
  | Input   of 'i expr_info
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
