(*
    result.v

    The result type from OCaml.
*)


Inductive result (A E:Set) : Set :=
    | Ok    : A -> result A E
    | Error : E -> result A E.


Arguments Ok {A E}.
Arguments Error {A E}.


Definition apply (A B E:Set) (f:B -> A -> A) (v:B) (r:result A E) : result A E :=
    match r with
    | Ok v' => Ok (f v v')
    | e => e
    end.

