module StrMap = Map.Make (String)

type typ =
  | TBits   of int
  | TRecord of typ StrMap.t

let tbits n =
  TBits n

let trecord_of_list tl =
  TRecord (StrMap.of_list tl)

let rec eq t1 t2 =
  match t1, t2 with
  | TBits n    , TBits m     -> n = m
  | TRecord tr1, TRecord tr2 -> StrMap.equal eq tr1 tr2
  | _ -> false
;;

module Pretty : sig
  val pp_t : typ Fmt.t
end = struct
  let rec pp_t ppf = function
    | TBits   n -> Fmt.pf ppf "bits %d" n
    | TRecord r -> pp_r ppf r

  and pp_f ppf v =
    Fmt.pair ~sep:(Fmt.any ":@ ")
      Fmt.string pp_t ppf v
  and pp_r ppf r =
    Fmt.braces (Fmt.list ~sep:Fmt.comma pp_f)
      ppf
      (StrMap.to_list r)
end
