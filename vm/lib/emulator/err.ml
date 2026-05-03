(*
    err.ml

    Errors.
*)


module A = Syntax.Ast
module I = Ir
module P = State.PP
module STbl = Hashtbl.Make (String)

module U = Utils.Parsing


(* String representation of an annotation with lexing position pair (ls, le),
   src_tbl being a hash table mapping paths to source code.
   NOTE: It is assumed that the annotation is over a single file only. *)
let annot_string src_tbl (ls, le) = Lexing.(
    let path = ls.pos_fname in
    let l1 = ls.pos_lnum and l2 = le.pos_lnum in
    let c1 = ls.pos_cnum - ls.pos_bol + 1 in
    let c2 = le.pos_cnum - le.pos_bol + 1 in
    let src = STbl.find src_tbl path in
    let ls = U.get_lines_with_markers '*' '*' src l1 l2 c1 c2 in
    let header =
        if l1 = l2 then Printf.sprintf
            "File \"%s\", line %d columns %d to %d" path l1 c1 c2
        else Printf.sprintf
            "File \"%s\", line %d column %d, to line %d column %d"
            path l1 c1 l2 c2
    in
    Printf.sprintf "%s\n%s" header ls)


(* Errors that indicate a violation of semantics. *)
module Semantic = struct
    type t =
        | DuplicateLabel    of string
        | UnboundLabel      of string
        | DuplicateFunc     of string
        | NoEntryFunc       of string
        | NoReturnInFunc    of string

    let to_string = function
        | DuplicateLabel l  -> "Duplicate label " ^ l
        | UnboundLabel l    -> "Unbound label " ^ l
        | DuplicateFunc f   -> "Duplicate function " ^ f
        | NoEntryFunc f     -> "Entry function " ^ f ^ " not found"
        | NoReturnInFunc f  ->
            "Function " ^ f ^ " does not end with a return instruction"
end


(* Errors during emulations of VM programs. *)
module Execution = struct
    (* TODO: Improve the arguments. Maybe add state as argument. *)
    type base =
        | NotEnoughValsOnWorkingStack   of int * int
        | ExecutionTerminated           of int
        | NoReturnValue
        | UnboundFunction               of string
        | OutOfBoundSegRead             of I.segment * int * int
        | OutOfBoundSegWrite            of I.segment * int * int
        | UnImplemented

    (* Pairing a base error up with the state that caused it. *)
    type t = State.t * base

    (* Extracting state and base from an error. *)
    let state = fst
    let base = snd

    let base_to_string = function
        | NotEnoughValsOnWorkingStack (n, n') -> Printf.sprintf
            "Not enough values on the working stack. Needed at least %d, got only %d" n n'
        | ExecutionTerminated ret_val -> Printf.sprintf
            "Execution terminated with return value %d" ret_val
        | NoReturnValue -> "Nothing in the working stack to return"
        | UnboundFunction f -> "Unbound function " ^ f
        | OutOfBoundSegRead (seg, n, n') -> Printf.sprintf
            "Attempted to read segment %s out of bounds"
            (Syntax.Printers.seg seg) ^ Printf.sprintf
            "\n\tSegment size is %d and attempted to read index %d" n n'
        | OutOfBoundSegWrite (seg, n, n') -> Printf.sprintf
            "Attempted to write segment %s out of bounds"
            (Syntax.Printers.seg seg) ^ Printf.sprintf
            "\n\tSegment size is %d and attempted to write index %d" n n'
        | UnImplemented -> "Unimplemented feature"

    let diagnostics_to_string = P.diagnostic
end


(* Errors in the REPL mode. *)
module Repl = struct
    type t =
        | EmptyCallStack
        | ExecuteTillReturnFromMain

    let to_string = function
        | EmptyCallStack ->
            "Call stack is empty"
        | ExecuteTillReturnFromMain ->
            "Can't perform execute till return from main"
end


(* Exceptions. *)
type exn +=
    | SemanticError  of A.annot option * Semantic.t
    | ExecutionError of I.annot * Execution.t
    | ReplError      of Repl.t


let pp_repl_error = function
    | ReplError e -> Some (Printf.sprintf
        "REPL error: %s." (Repl.to_string e))
    | _ -> None

let () = Printexc.register_printer pp_repl_error


(* Printers for exceptions.
   src_table is a hash table from path to source code. *)

let pp_semantic_error src_tbl = function
    | (None, e) -> Printf.sprintf "Semantic error: %s."
        (Semantic.to_string e)
    | (Some a, e) -> Printf.sprintf "%s\nSemantic error: %s."
        (annot_string src_tbl a) (Semantic.to_string e)

let pp_execution_error src_tbl (a, e) = Execution.(
    Printf.sprintf "%s\nExecution error: %s."
    (annot_string src_tbl a) (base_to_string (base e)))

let pp_execution_error_verbose src_tbl (a, e) = Execution.(
    Printf.sprintf "%s\nExecution error:\n%s.\n\nState diagnostic info:\n%s"
    (annot_string src_tbl a) (base_to_string (base e))
    (diagnostics_to_string (state e)))



(* Helper functions (and values) for raising exceptions. *)
module Helper = struct
    module Semantic = struct
        open Semantic

        let exn_with_inst i e = SemanticError (Some (A.inst_annot i), e)
        let exn_with_func f e = SemanticError (Some (A.func_annot f), e)
        let exn_with_annot a e = SemanticError (Some a, e)

        let dup_label i l = exn_with_inst i (DuplicateLabel l)
        let unbound_label i l = exn_with_inst i (UnboundLabel l)

        let dup_func a dup_f = exn_with_annot a (DuplicateFunc dup_f)
        let no_return f fn = exn_with_func f (NoReturnInFunc fn)

        let no_entry_func main_f = SemanticError (None, NoEntryFunc main_f)
    end

    module Execution = struct
        open Execution

        let exn_with_inst i s e = ExecutionError (I.inst_annot i, (s, e))
        let exn_with_func i s e = ExecutionError (I.func_annot i, (s, e))

        let not_enough_vals i s n n' = exn_with_inst i s
            (NotEnoughValsOnWorkingStack (n, n'))
        let execution_terminated i s v = exn_with_inst i s (ExecutionTerminated v)
        let no_ret_val i s = exn_with_inst i s NoReturnValue
        let unbound_func i s f = exn_with_inst i s (UnboundFunction f)
        let unimplemented i s = exn_with_inst i s UnImplemented
        let oob_seg_read i s seg n n' = exn_with_inst i s
            (OutOfBoundSegRead (seg, n, n'))
        let oob_seg_write i s seg n n' = exn_with_inst i s
            (OutOfBoundSegWrite (seg, n, n'))
    end

    module Repl = struct
        open Repl

        let empty_call_stack = ReplError EmptyCallStack
        let e_till_return_from_main = ReplError ExecuteTillReturnFromMain
    end
end

