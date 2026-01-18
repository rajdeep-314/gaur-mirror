(*
    result.v

    The result type from OCaml.
*)


Inductive result (A E:Set) : Set :=
    | Ok    : A -> result A E
    | Error : E -> result A E.


Arguments Ok {A E}.
Arguments Error {A E}.


(* Applies f to r if it's Ok, else gives the same error. *)
Definition apply {A B E:Set} (f:A -> B) (r:result A E) : result B E :=
    match r with
    | Ok v      => Ok (f v)
    | Error e   => Error e
    end.

