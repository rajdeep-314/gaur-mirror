(*
    err.ml

    Errors.
*)


module Semantic = struct
    type t =
        | DuplicateLabel    of string
        | UnboundLabel      of string

    let to_string = function
        | DuplicateLabel l  -> "Duplicate label " ^ l
        | UnboundLabel l    -> "Unbound label " ^ l
end


type exn +=
    | SemanticError of Semantic.t


(* Helper functions for raising exceptions. *)
module Helper = struct
    let dup_label l = SemanticError (Semantic.DuplicateLabel l)
    let unbound_label l = SemanticError (Semantic.UnboundLabel l)
end

