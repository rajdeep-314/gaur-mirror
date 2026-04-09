open Parse
open Hdlutils

type t =
  | Splice_bounds of int * int * int
  | Splice_type   of Types.t
  | Concat_type   of Types.t * Types.t
  | Access        of string
  | Member_field  of string
  | Member_type   of Types.t

exception TypeError of Loc.t * t

let raise_splice_bounds loc lb ub l =
  raise (TypeError (loc, Splice_bounds (lb, ub, l)))

let raise_splice_type loc typ =
  raise (TypeError (loc, Splice_type (typ)))

let raise_concat_type loc typ1 typ2 =
  raise (TypeError (loc, Concat_type (typ1, typ2)))

let raise_access loc id =
  raise (TypeError (loc, Access id))

let raise_member_field loc fld =
  raise (TypeError (loc, Member_field fld))

let raise_member_type loc typ =
  raise (TypeError (loc, Member_type typ))

module Pertty = struct
  open Types.Pretty

  let pp ppf err =
    match err with
    | Splice_bounds (lb, ub, l)  ->
      Fmt.pf ppf
        "Splice indices [%d, %d] out of range [0, %d]" lb ub (l - 1)

    | Splice_type t ->
      Fmt.pf ppf "Can not splice an expression of type %a" pp_t t

    | Concat_type (t1, t2) ->
      Fmt.pf ppf "Can not concatenate expressions of types %a and %a"
        pp_t t1
        pp_t t2

    | Access id ->
      Fmt.pf ppf "Unbound variable %s" id

    | Member_field fld ->
      Fmt.pf ppf "The record has no field '%s'" fld

    | Member_type t ->
      Fmt.pf ppf "The expression has type %a which is not a record"
        pp_t t
end
