(*
  printers.ml

  Printers for VM instructions, functions and programs.
*)

open Ast


(* Convert a segment to its string representation. *)
let seg (seg: segment) =
    match seg with
    | Temp      -> "temp"
    | Static    -> "static"
    | Local     -> "local"
    | Argument  -> "argument"

let syscall = function
    | PutChar   -> "putchar"

(* Convert a VM instruction to its string representation. *)
let inst_base sl sf inst =
    match inst with
    | Push (s, i)       -> Printf.sprintf "push %s %d" (seg s) i
    | Pop  (s, i)       -> Printf.sprintf "pop %s %d"  (seg s) i
    | PushC n           -> "push constant " ^ string_of_int n
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

let inst sl sf (a, i) = inst_base sl sf i

(* Convert a list of VM instructions to their string representation. *)
let insts sl sf insts =
    List.map (inst sl sf) insts

(* Tabulate a list of strings by adding a tab character at the start of each line. *)
let tabulate (lines: string list) : string list =
    List.map (fun line -> "\t" ^ line) lines

(* Convert a VM function to its string representation. *)
let func sl sf f =
    let header = Printf.sprintf "function %s %d" (sf f.f_name) f.n_vars in
    let body_lines = tabulate (insts sl sf f.body) in
    header :: body_lines

(* Convert a VM program to its string representation. *)
let prog sl sf prog =
    let func_lines = List.flatten (List.map (func sl sf) prog) in
    String.concat "\n" func_lines 

