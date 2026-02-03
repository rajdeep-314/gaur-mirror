(*
    err.v

    Errors that the VM translator might raise.
*)


(* Errors where the VM labels are from L. *)
Variant t (L:Set) : Set :=
    | PopToConst
    | UnboundLabel
    | UnboundFunction
    | DuplicateLabel (l:L).

