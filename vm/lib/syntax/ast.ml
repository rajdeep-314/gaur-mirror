(*
    ast.ml

    AST of VM programs.
*)


(* VM segments. *)
type segment =
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
    (* pushing constants. *)
    | PushC of int
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

