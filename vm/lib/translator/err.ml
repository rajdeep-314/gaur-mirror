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

let to_str (sl: 'l -> string) (err: 'l err) : string =
    match err with
    | PopToConst            -> "Error: Attempted to pop to constant segment."
    | UnboundLabel          -> "Error: Unbound label."
    | UnboundFunction       -> "Error: Unbound function."
    | DuplicateLabel l      -> Printf.sprintf "Error: Duplicate label '%s'." (sl l)
    | NoMainFunction        -> "Error: No 'main' function found." 
    | MultipleMainFunctions -> "Error: Multiple 'main' functions found."


