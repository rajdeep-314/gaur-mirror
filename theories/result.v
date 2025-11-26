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

(* Applies the function f to v and to a result, if the result is Ok. Else,
   returns the same error. *)
(* NOTE:    Maybe it's possible to express apply2 in terms of apply. *)
Definition apply2 (A B E:Set) (f:B -> A -> A) (v:B) (r:result A E) : result A E :=
    match r with
    | Ok v' => Ok (f v v')
    | e     => e
    end.


