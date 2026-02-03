(*
   types.ml

   Types for VM instructions, functions and programs.
*)


(* VM segments. *)
type segment =
    | Constant
    | Temp
    | Static
    | Local
    | Argument

(* VM instructions with label names and function names from 'l
   and 'f, respectively. *)
type ('l, 'f) inst =
    (* stack operations *)
    | Push of segment * int
    | Pop  of segment * int
    (* arithmetic operations *)
    | Add | Sub | Neg
    (* comparison operations *)
    | Eq  | Gt  | Lt
    (* logical operations *)
    | And | Or  | Not
    (* label related operations *)
    | Label  of 'l
    | Goto   of 'l
    | IfGoto of 'l
    (* function related operations *)
    | Call of 'f * int
    | Return

(* List of VM instructions (function bodies are this type). *)
type ('l,'f) insts = ('l,'f) inst list

(* VM function -- name, number of local variables and body. *)
type ('l,'f) func = {
    f_name : 'f;
    n_vars : int;
    body   : ('l,'f) insts;
}

(* Type for VM programs -- lists of VM functions. *)
type ('l,'f) prog = ('l,'f) func list

(* Errors that the VM translator can produce.
   'l is the type of VM label names. *)
type 'l err =
    | PopToConst
    | UnboundLabel
    | UnboundFunction
    | DuplicateLabel of 'l
    | NoMainFunction
    | MultipleMainFunctions

