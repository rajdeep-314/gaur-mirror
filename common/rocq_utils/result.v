(*
    result.v
*)

(** {1 Utility functions related to the result type} *)

(** [result_map f r] returns [Ok (f v)] if [r] is [Ok v], and if [r] is
    [Error e], then [Error e] is returned. *)
Definition result_map {A B E:Type} (f:A -> B) (r:result A E) : result B E :=
    match r with
    | Ok v    => Ok (f v)
    | Error e => Error e
    end.

