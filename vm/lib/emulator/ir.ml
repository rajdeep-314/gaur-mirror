(*
    ir.ml

    IR is similar to VM, except:
    - There are no labels.
*)


module A = Syntax.Emulator.Ast
module W = Types.Word


type segment = A.segment        (* IR segments are VM segments. *)
type syscall = A.syscall        (* IR syscalls are VM syscalls. *)

(* IR instructions (without any annotations). *)
type inst_base =
    (* stack operations *)
    | Push of segment * int
    | Pop  of segment * int
    (* pushing constants. *)
    | PushC of W.t
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
    (* breakpoints *)
    | LineBpt   of string * (relop * int) option
    | CondBpt   of string * cond_exp

and exp = A.exp =
    | Const         of W.t
    | StackOffset   of int
    | SegmentOffset of segment * int
    | BinOp         of binop * exp * exp
    | Neg           of exp

and cond_exp = A.cond_exp =
    | True | False
    | Comp      of relop * exp * exp
    | BoolOp    of boolop * cond_exp * cond_exp
    | Not       of cond_exp

and relop  = A.relop  = R_eq | R_lt | R_gt | R_le | R_ge | R_ne
and binop  = A.binop  = B_add | B_sub | B_mul | B_div
and boolop = A.boolop = Bl_and | Bl_or

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
type 'a prog = {
    main_f : string ;
    funcs  : (string, 'a func) Hashtbl.t
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

    let inst_base = function
        | Push (s, i)   -> Printf.sprintf "push %s %d" (seg s) i
        | Pop  (s, i)   -> Printf.sprintf "pop %s %d"  (seg s) i
        | PushC n       -> "push constant " ^ W.to_string n
        | Add           -> "add"
        | Sub           -> "sub"
        | Neg           -> "neg"
        | Eq            -> "eq"
        | Gt            -> "gt"
        | Lt            -> "lt"
        | And           -> "and"
        | Or            -> "or"
        | Not           -> "not"
        | Goto i        -> Printf.sprintf "goto %d" i
        | IfGoto i      -> Printf.sprintf "if-goto %d" i
        | Call (f, n)   -> Printf.sprintf "call %s %d" f n
        | SCall f       -> Printf.sprintf "scall %s" (syscall f)
        | Return        -> "return"
        | LineBpt (s, None) -> Printf.sprintf "%%l %s" s
        | CondBpt (s, c)    -> Printf.sprintf "%%l %s(%s)" s (cond_exp c)
        | LineBpt (s, Some (o, n)) -> Printf.sprintf
            "%%l %s(%s %d)" s (relop o) n

    let inst i = inst_base (fst i)
    let insts is =
        String.concat "\n" (List.map inst (Array.to_list is))

    let func f = Printf.sprintf "function %s %d\n%s"
    f.f_name f.n_vars (insts f.body)

    let prog p =
        let fs = List.of_seq (Hashtbl.to_seq_values p.funcs) in
        Printf.sprintf "Entry function: %s\n%s" p.main_f
            (String.concat "\n" (List.map func fs))
end

