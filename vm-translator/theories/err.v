(*
    err.v

    Errors that the VM translator might raise.
*)


Variant t:Set :=
    | PopToConst
    | UnboundLabel
    | NotImplemented.

