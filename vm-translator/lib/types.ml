(*
   types.ml

   Types for VM instructions, functions and programs.
*)

type segment =
    | Constant
    | Temp
    | Static
    | Local
    | Argument

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

type ('l,'f) insts = ('l,'f) inst list

type ('l,'f) func = {
    f_name : 'f;
    n_vars : int;
    body   : ('l,'f) insts;
}

type ('l,'f) prog = ('l,'f) func list
