module StrMap = Map.Make(String)

type ident = string

type t =
  | TBits     of int
  | TRecord   of t StrMap.t
  | TChip     of t * t

let rec eq t1 t2 =
  match t1, t2 with
  | TBits n, TBits m -> n == m

  | TRecord r1, TRecord r2 -> StrMap.equal eq r1 r2

  | TChip (tin1, tout1), TChip (tin2, tout2) -> (eq tin1  tin2 ) &&
                                                (eq tout1 tout2)

  | _ -> false

type state_desc =
  | Current
  | Next

type port =
  { port_name : ident;
    port_type : t
  }

module Pretty : sig
  val pp_dump_t : t Fmt.t
end = struct
  let arrow = Fmt.any " ->@ "
  let rec pp_dump_t ppf = function
    | TBits n           -> Fmt.pf ppf "bits %d" n

    | TRecord r         -> Fmt.braces
                             (Fmt.list ~sep:Fmt.comma pp_f)
                             ppf
                             (StrMap.to_list r)

    | TChip (tin, tout) -> Fmt.hovbox (Fmt.pair ~sep:arrow pp_dump_t pp_dump_t)
                             ppf
                             (tin, tout)

  and pp_f ppf v = Fmt.pair ~sep:(Fmt.any ":@ ") (Fmt.string) (pp_dump_t) ppf v
end
