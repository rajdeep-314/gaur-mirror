module StrMap = Map.Make(String)

type ident = string

type typ =
  | TBits of int
  (** bits n *)
  | TRecord of typ StrMap.t
  (** {f1: T, f2: T, ...} *)
  | TChip of typ * typ
  (** And, Or, Mux, Register *)

let rec typ_equal typ1 typ2 =
  match typ1, typ2 with
  | TBits n, TBits m ->
    n == m
  | TRecord r1, TRecord r2 ->
    StrMap.equal typ_equal r1 r2
  | TChip (tin1, tout1), TChip (tin2, tout2) ->
    (typ_equal tin1 tin2) && (typ_equal tout1 tout2)
  | _ -> false

type state_desc =
  | Current
  (* Current-state binding *)
  | Next
  (* Next-State binding *)

type port =
  { port_name : ident;
    port_type : typ
  }
