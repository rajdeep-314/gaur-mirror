(*
    ir.ml

    IR is similar to VM, except:
    - There are no labels.
*)


module A = Syntax.Ast_new


type segment = A.segment        (* IR segments are VM segments. *)
type syscall = A.syscall        (* IR syscalls are VM syscalls. *)

(* IR instructions (without any annotations). *)
type inst_base =
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
    | Call  of string * int
    | SCall of syscall
    | Return

(* IR instructions with annotations of type 'a. *)
type 'a inst = inst_base * 'a
type 'a insts = 'a inst array

(* IR functions. *)
type 'a func = {
    f_name : string ;
    n_vars : int ;
    body   : 'a insts ;
    annot   : 'a
}


(* IR programs. *)
(* TODO: Maybe remove f_lbls. *)
type 'a prog = {
    main_f : string ;
    funcs  : (string, 'a func) Hashtbl.t ;
    f_lbls : (string, string list) Hashtbl.t
}


(* Default types. *)
type annot = A.annot
type def_inst = annot inst
type def_insts = annot insts
type def_func = annot func
type def_prog = annot prog


(* Extracting annotations and base instructions. *)
let inst_base : 'a inst -> inst_base = fst
let inst_annot : 'a inst -> 'a = snd
let func_annot f = f.annot

