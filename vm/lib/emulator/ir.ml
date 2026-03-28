(*
    ir.ml

    IR is similar to VM, except:
    - There are no labels.
*)


(* IR segments are VM segments. *)
type segment = Syntax.Ast.segment

(* IR instructions. *)
type inst =
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
    | Goto   of int
    | IfGoto of int
    (* function related operations *)
    | Call of string * int
    | Return

type insts = inst array

(* IR functions. *)
type func = {
    f_name : string ;
    n_vars : int ;
    body   : insts
}


(* IR programs. *)
type prog = {
    main_f : string ;
    funcs  : (string, func) Hashtbl.t ;
    f_lbls : (string, string list) Hashtbl.t
}

