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


module Execution = struct
    type t =
        | EmptyCallStack
        | NotEnoughArgsOnWorkingStack   of int * int
        | UnImplemented
        (*| OutOfBoundSegRead     of Ir.segment * int * int*)
        (*| OutOfBoundSegWrite    of Ir.segment * int * int*)

    let to_string = function
        | EmptyCallStack -> "Empty call stack"
        | NotEnoughArgsOnWorkingStack (n, n') -> Printf.sprintf
            "Not enough arguments on the working stack. Needed at least %d, got only %d" n n'
        | UnImplemented -> "Unimplemented feature."
        (*| OutOfBoundSegRead (seg, n, n') -> Printf.sprintf*)
        (*    "Attempted to read segment %s out of bounds."*)
        (*    (Syntax.Printers.str_of_seg seg) ^ Printf.sprintf*)
        (*    "\n\tSegment size is %d and attempted to read index %d" n n'*)
        (*| OutOfBoundSegWrite (seg, n, n') -> Printf.sprintf*)
        (*    "Attempted to write segment %s out of bounds."*)
        (*    (Syntax.Printers.str_of_seg seg) ^ Printf.sprintf*)
        (*    "\n\tSegment size is %d and attempted to write index %d" n n'*)
end


type exn +=
    | SemanticError  of Semantic.t
    | ExecutionError of Execution.t


(* Helper functions (and values) for raising exceptions. *)
module Helper = struct
    module Semantic = struct
        let dup_label l = SemanticError (Semantic.DuplicateLabel l)
        let unbound_label l = SemanticError (Semantic.UnboundLabel l)
    end

    module Execution = struct
        let empty_call_stack = ExecutionError (Execution.EmptyCallStack)
        let not_enough_args n n' =
            ExecutionError (Execution.NotEnoughArgsOnWorkingStack (n, n'))
        let unimplemented = ExecutionError (Execution.UnImplemented)
        (*let oob_seg_read s n n' =*)
        (*        ExecutionError (Execution.OutOfBoundSegRead (s, n, n'))*)
        (*let oob_seg_write s n n' =*)
        (*        ExecutionError (Execution.OutOfBoundSegWrite (s, n, n'))*)
    end
end

