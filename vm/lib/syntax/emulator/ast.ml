(*
    ast.ml

    AST of VM emulator programs.
*)


module A = Vm.Ast
module W = Types.Word


(* VM segments. *)
type segment = A.segment =
    | Temp
    | Static
    | Local
    | Argument

(* Supported system calls. *)
type syscall = A.syscall =
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
    (* breakpoints *)
    | LineBpt   of string * (relop * int) option
    | CondBpt   of string * cond_exp

and exp =
    | Const         of W.t
    | StackOffset   of int
    | SegmentOffset of segment * int
    | BinOp         of binop * exp * exp
    | Neg           of exp

and cond_exp =
    | True | False
    | Comp      of relop * exp * exp
    | BoolOp    of boolop * cond_exp * cond_exp
    | Not       of cond_exp

and relop = R_eq | R_lt | R_gt | R_le | R_ge | R_ne
(* TODO: Add more binary operators. *)
and binop = B_add | B_sub | B_mul | B_div
(* TODO: Add more boolean operators. *)
and boolop = Bl_and | Bl_or



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
    let seg = A.PP.seg
    let syscall = A.PP.syscall

    let relop = function
        | R_eq -> "="  | R_lt -> "<"  | R_gt -> ">"
        | R_le -> "<=" | R_ge -> ">=" | R_ne -> "<>"

    let binop = function
        | B_add -> "+" | B_sub -> "-" | B_mul -> "*" | B_div -> "/"

    let boolop = function
        | Bl_and -> "&&" | Bl_or -> "||"

    let rec exp = function
        | Const n       -> W.to_string n
        | StackOffset n -> Printf.sprintf "stack[%d]" n
        | Neg (Const n) -> Printf.sprintf "-%s" (W.to_string n)
        | Neg e         -> Printf.sprintf "-(%s)" (exp e)
        | SegmentOffset (s, n) -> Printf.sprintf "%s[%d]" (seg s) n
        | BinOp (o, e1, e2) -> Printf.sprintf
            "(%s %s %s)" (exp e1) (binop o) (exp e2)

    let rec cond_exp = function
        | True      -> "true"  | False      -> "false"
        | Not True  -> "~true" | Not False  -> "~false"
        | Comp (o, e1, e2) -> Printf.sprintf "(%s %s %s)"
            (exp e1) (relop o) (exp e2)
        | Not e -> Printf.sprintf "~(%s)" (cond_exp e)
        | BoolOp (o, c1, c2) -> Printf.sprintf "(%s %s %s)"
            (cond_exp c1) (boolop o) (cond_exp c2)

    (* Convert a VM instruction to its string representation. *)
    let inst_base sl sf inst =
        match inst with
        | Push (s, i)       -> Printf.sprintf "push %s %d" (seg s) i
        | Pop  (s, i)       -> Printf.sprintf "pop %s %d"  (seg s) i
        | PushC n           -> "push constant " ^ W.to_string n
        | PushKeyboard      -> "push keyboard"
        | Add               -> "add"
        | Sub               -> "sub"
        | Neg               -> "neg" 
        | Eq                -> "eq"
        | Gt                -> "gt"  
        | Lt                -> "lt"
        | And               -> "and"
        | Or                -> "or"
        | Not               -> "not"
        | Label l           -> Printf.sprintf "label %s" (sl l)
        | Goto l            -> Printf.sprintf "goto %s" (sl l)
        | IfGoto l          -> Printf.sprintf "if-goto %s" (sl l)
        | Call (f, n)       -> Printf.sprintf "call %s %d" (sf f) n
        | SCall f           -> Printf.sprintf "scall %s" (syscall f)
        | Return            -> "return"
        | LineBpt (s, None) -> Printf.sprintf "%%l %s" s
        | CondBpt (s, c)    -> Printf.sprintf "%%l %s(%s)" s (cond_exp c)
        | LineBpt (s, Some (o, n)) -> Printf.sprintf
            "%%l %s(%s %d)" s (relop o) n

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

