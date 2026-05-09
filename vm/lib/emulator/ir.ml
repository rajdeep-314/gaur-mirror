(*
    ir.ml

    IR is similar to VM, except:
    - There are no labels.
*)


module A = Syntax.Vm_ast


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

    let inst_base = function
        | Push (s, i)   -> Printf.sprintf "push %s %d" (seg s) i
        | Pop  (s, i)   -> Printf.sprintf "pop %s %d"  (seg s) i
        | PushC n       -> "push constant " ^ string_of_int n
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

