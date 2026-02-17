module StrMap = Map.Make(String)

type ident = string

type t =
  | TBits     of int
  (** bits n *)
  | TRecord   of t StrMap.t
  (** {f1: T, f2: T, ...} *)
  | TChip     of t * t
  (** And, Or, Mux, Register *)

let rec eq t1 t2 =
  match t1, t2 with
  | TBits n, TBits m -> n == m

  | TRecord r1, TRecord r2 -> StrMap.equal eq r1 r2

  | TChip (tin1, tout1), TChip (tin2, tout2) -> (eq tin1  tin2 ) &&
                                                (eq tout1 tout2)

  | _ -> false

type state_desc =
  | Current
  (* Current-state binding *)
  | Next
  (* Next-State binding *)

type port =
  { port_name : ident;
    port_type : t
  }
