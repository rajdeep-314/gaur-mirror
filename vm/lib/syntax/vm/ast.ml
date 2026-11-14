(*
    ast.ml

    AST of VM programs.
*)


module W = Types.Word


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
    | PushC of W.t
    (* push keyboard content to working stack. *)
    | PushKeyboard
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


(* Pretty-printers. *)
module PP = struct
    let seg = function
        | Temp      -> "temp"
        | Static    -> "static"
        | Local     -> "local"
        | Argument  -> "argument"

    let syscall = function
        | PutChar   -> "putchar"

    (* Convert a VM instruction to its string representation. *)
    let inst_base sl sf inst =
        match inst with
        | Push (s, i)   -> Printf.sprintf "push %s %d" (seg s) i
        | Pop  (s, i)   -> Printf.sprintf "pop %s %d"  (seg s) i
        | PushC n       -> "push constant " ^ W.to_string n
        | PushKeyboard  -> "push keyboard"
        | Add           -> "add"
        | Sub           -> "sub"
        | Neg           -> "neg" 
        | Eq            -> "eq"
        | Gt            -> "gt"  
        | Lt            -> "lt"
        | And           -> "and"
        | Or            -> "or"
        | Not           -> "not"
        | Label l       -> Printf.sprintf "label %s" (sl l)
        | Goto l        -> Printf.sprintf "goto %s" (sl l)
        | IfGoto l      -> Printf.sprintf "if-goto %s" (sl l)
        | Call (f, n)   -> Printf.sprintf "call %s %d" (sf f) n
        | SCall f       -> Printf.sprintf "scall %s" (syscall f)
        | Return        -> "return"

    let inst sl sf (a, i) = inst_base sl sf i

    (* Convert a list of VM instructions to their string representation. *)
    let insts sl sf insts =
        List.map (inst sl sf) insts

    (* Tabulate a list of strings by adding a tab character at the start of
       each line. *)
    let tabulate lines =
        List.map ((^) "\t") lines

    (* Convert a VM function to its string representation. *)
    let func sl sf f =
        let header = Printf.sprintf "function %s %d" (sf f.f_name) f.n_vars in
        let body_lines = tabulate (insts sl sf f.body) in
        header :: body_lines

    (* Convert a VM program to its string representation. *)
    let prog sl sf prog =
        let func_lines = List.flatten (List.map (func sl sf) prog) in
        String.concat "\n" func_lines 
end

