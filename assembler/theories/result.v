(*
    result.v
*)

(**
    {1 Result Module}

    This module defines a result type similar to that in OCaml,
    which can represent either a successful value or an error.
*)


(** Inductive type definition for result. *)
Inductive result (A E:Set) : Set :=
    | Ok    : A -> result A E
    | Error : E -> result A E.


(** Implicit Arguments. *)
Arguments Ok {A E}.
Arguments Error {A E}.

(**
 [apply f r] applies function [f] to the value inside [r] if [r] is [Ok v],
 otherwise it returns the same [Error e].
*)
Definition apply {A B E:Set} (f:A -> B) (r:result A E) : result B E :=
    match r with
    | Ok v      => Ok (f v)
    | Error e   => Error e
    end.

