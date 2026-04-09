(*
    err.ml

    Errors.
*)


(* Errors that indicate a violation of semantics. *)
module Semantic = struct
    type t =
        | DuplicateLabel    of string
        | UnboundLabel      of string
        | DuplicateFunc     of string

    let to_string = function
        | DuplicateLabel l  -> "Duplicate label " ^ l
        | UnboundLabel l    -> "Unbound label " ^ l
        | DuplicateFunc f   -> "Duplicate function " ^ f
end


(* Errors during emulations of VM programs. *)
module Execution = struct
    type t =
        | EmptyCallStack
        | NotEnoughValsOnWorkingStack   of int * int
        | ExecutionTerminated           of int
        | UnboundFunction               of string
        | OutOfBoundSegRead             of Ir.segment * int * int
        | OutOfBoundSegWrite            of Ir.segment * int * int
        | OutOfBoundPC                  of string * int * int
        | UnImplemented

    let to_string = function
        | EmptyCallStack -> "Empty call stack"
        | NotEnoughValsOnWorkingStack (n, n') -> Printf.sprintf
            "Not enough values on the working stack. Needed at least %d, got only %d" n n'
        | ExecutionTerminated ret_val -> Printf.sprintf
            "Execution terminated with return value %d." ret_val
        | UnboundFunction f -> "Unbound function " ^ f
        | OutOfBoundSegRead (seg, n, n') -> Printf.sprintf
            "Attempted to read segment %s out of bounds."
            (Syntax.Printers.seg seg) ^ Printf.sprintf
            "\n\tSegment size is %d and attempted to read index %d" n n'
        | OutOfBoundSegWrite (seg, n, n') -> Printf.sprintf
            "Attempted to write segment %s out of bounds."
            (Syntax.Printers.seg seg) ^ Printf.sprintf
            "\n\tSegment size is %d and attempted to write index %d" n n'
        | OutOfBoundPC (f, n_insts, n) -> Printf.sprintf
            "Attempted to fetch instruction number %d in function %s, which \
            only has %d instructions" n f n_insts
        | UnImplemented -> "Unimplemented feature."
end


(* Errors in the REPL mode. *)
module Repl = struct
    type t =
        | EmptyCallStack
        | ExecuteTillReturnFromMain

    let to_string = function
        | EmptyCallStack ->
            "Call stack is empty."
        | ExecuteTillReturnFromMain ->
            "Can't perform execute till return from main."
end


type exn +=
    | SemanticError  of Semantic.t
    | ExecutionError of Execution.t
    | ReplError      of Repl.t


(* Helper functions (and values) for raising exceptions. *)
module Helper = struct
    module Semantic = struct
        open Semantic

        let dup_label l = SemanticError (DuplicateLabel l)
        let unbound_label l = SemanticError (UnboundLabel l)
        let dup_func f = SemanticError (DuplicateFunc f)
    end

    module Execution = struct
        open Execution

        let empty_call_stack = ExecutionError EmptyCallStack
        let not_enough_vals n n' =
            ExecutionError (NotEnoughValsOnWorkingStack (n, n'))
        let execution_terminated v =
            ExecutionError (ExecutionTerminated v)
        let unbound_func f =
            ExecutionError (UnboundFunction f)
        let unimplemented = ExecutionError UnImplemented
        let oob_seg_read s n n' =
            ExecutionError (OutOfBoundSegRead (s, n, n'))
        let oob_seg_write s n n' =
            ExecutionError (OutOfBoundSegWrite (s, n, n'))
        let oob_pc f n_insts n =
            ExecutionError (OutOfBoundPC (f, n_insts, n))
    end

    module Repl = struct
        open Repl

        let empty_call_stack = ReplError EmptyCallStack
        let e_till_return_from_main = ReplError ExecuteTillReturnFromMain
    end
end

