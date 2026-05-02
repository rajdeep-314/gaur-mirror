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

(* Supported system calls. *)
type syscall =
    | PutChar

(* VM instructions with label names and function names from 'l and 'f,
   respectively. *)
type ('l, 'f) inst_base =
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
    | Call   of 'f * int
    | SCall  of syscall
    | Return


(* Instruction, annotated with something of type 'a. *)
type ('a, 'l, 'f) inst = ('l, 'f) inst_base * 'a
type ('a, 'l, 'f) insts = ('a, 'l, 'f) inst list

(* A function has a name, a count of local variables to be used, and a body. *)
type ('a, 'l, 'f) func = {
    f_name  : 'f ;
    n_vars  : int ;
    body    : ('a, 'l, 'f) insts ;
    annot   : 'a
}

(* A program is a list of functions, essentially. *)
type ('a, 'l, 'f) prog = ('a, 'l, 'f) func list


(* "Default" types: types annotated with a pair of lexing positions, with
   labels and function names being strings. *)

type annot = Lexing.position * Lexing.position

type def_inst = (annot, string, string) inst
type def_insts = (annot, string, string) insts list
type def_func = (annot, string, string) func
type def_prog = (annot, string, string) prog


(* Functions to extract annotations and base instructions. *)

(* Bases. *)
let base_inst = fst
let base_insts is = List.map base_inst is
let base_body f = base_insts f.body

(* Annotations. *)
let inst_annot = snd
let func_annot f = f.annot

