(*
    vm2ir.ml

    Convert VM programs to IR programs.
*)


(* Module aliases. *)
module A = Syntax.Ast           (* VM AST. *)
module I = Ir                   (* IR AST. *)
module MU = Utils.Misc          (* Miscellaneous utils. *)

open Err.Helper.Semantic

(* (string, int) pairs to eliminate labels. *)
module Table = Map.Make (String)
type tbl = int Table.t


(* Make a table of label names and line numbers from a list of instructions. *)
let rec make_table ln_num is =
    match is with
    | []        -> Table.empty
    | A.Label l :: is' ->
        let tbl = make_table ln_num is' in
        begin match Table.find_opt l tbl with
        | Some _ -> raise (dup_label l)
        | None   -> Table.add l ln_num tbl end
    | _ :: is'  -> make_table (ln_num + 1) is'


(*
Translate a VM instruction to a list of IR instructions.
tbl :   table of label names and line numbers
ln  :   current line number
*)
let translate_inst tbl ln = function
    | A.Push (s, n) -> [I.Push (s, n)]
    | A.Pop (s, n)  -> [I.Pop (s, n)]
    | A.PushC n     -> [I.PushC n]
    | A.Add -> [I.Add] | A.Sub -> [I.Sub] | A.Neg -> [I.Neg]
    | A.Eq -> [I.Eq]   | A.Lt -> [I.Lt]   | A.Gt -> [I.Gt]
    | A.And -> [I.And] | A.Or -> [I.Or]   | A.Not -> [I.Not]
    | A.Label _ -> []
    | A.Call (f, n) -> [I.Call (f, n)]
    | A.Return -> [I.Return]
    | A.Goto l ->
        begin match Table.find_opt l tbl with
        | Some ln' -> [I.Goto (ln' - ln)]
        | None -> raise (unbound_label l) end
    | A.IfGoto l ->
        begin match Table.find_opt l tbl with
        | Some ln' -> [I.IfGoto (ln' - ln)]
        | None -> raise (unbound_label l) end

(* Translate a list of VM instructions to a list of IR instructions. *)
let rec translate_insts tbl ln is =
    match is with
    | []        -> []
    | i :: is'  ->
        let ln_incr =
            match i with
            | A.Label _ -> 0
            | _ -> 1 in
        translate_inst tbl ln i @ translate_insts tbl (ln + ln_incr) is'

(* Gets a list of labels used as targets in function f's body. *)
let labels_used_in_func f =
    List.filter_map
        (function | A.IfGoto l | A.Goto l -> Some l | _ -> None) f.A.body


(* Translate a VM function to an IR function, and a list of target labels. *)
let translate_func f =
    let tbl = make_table 0 f.A.body in
    let body = Array.of_list (translate_insts tbl 0 f.A.body) in
    (I.({ f_name = f.f_name; n_vars = f.n_vars; body = body }),
    labels_used_in_func f)

(* Translate a bunch of VM programs to an IR program. *)
let translate_progs main_f ps =
    (* One big program. *)
    let p = List.concat ps in
    (* List of function bodies and target labels in those functions. *)
    let fs, ls = List.split (List.map translate_func p) in
    (* List of function names. *)
    let fns = List.map (fun f -> f.I.f_name) fs in
    (* Checking for duplicate function names. *)
    match MU.find_duplicate fns with
    | Some f    -> raise (dup_func f)
    | None      ->
        Ir.{ main_f = main_f ;
             funcs  = MU.hashtbl_of_list (List.combine fns fs) ;
             f_lbls = MU.hashtbl_of_list (List.combine fns ls) }

(* Translate a VM program to an IR program. *)
let translate_prog main_f p = translate_progs main_f [p]

