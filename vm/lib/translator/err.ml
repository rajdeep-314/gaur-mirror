(*
    err.ml

    Errors that can arise during VM to ASM translations.
*)


(* 'l is the type of VM label names. *)
type 'l err =
    | PopToConst
    | UnboundLabel
    | UnboundFunction
    | DuplicateLabel of 'l
    | NoMainFunction
    | MultipleMainFunctions

