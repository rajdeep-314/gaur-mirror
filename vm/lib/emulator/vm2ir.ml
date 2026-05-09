(*
    vm2ir.ml

    Convert VM programs to IR programs.
*)


(* Module aliases. *)
module A = Syntax.Vm_ast        (* VM AST. *)
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
    | (A.Label l, _) as i :: is' ->
        let tbl = make_table ln_num is' in
        begin match Table.find_opt l tbl with
        | Some _ -> raise (dup_label i l)
        | None   -> Table.add l ln_num tbl end
    | _ :: is'  -> make_table (ln_num + 1) is'


(*
Translate a VM instruction to a list of IR instructions.
tbl :   table of label names and line numbers
ln  :   current line number
*)
let translate_inst tbl ln i =
    let base = match A.base_inst i with
    | A.Push (s, n) -> Some (I.Push (s, n))
    | A.Pop (s, n)  -> Some (I.Pop (s, n))
    | A.PushC n     -> Some (I.PushC n)
    | A.Add -> Some I.Add | A.Sub -> Some I.Sub | A.Neg -> Some I.Neg
    | A.Eq -> Some I.Eq   | A.Lt -> Some I.Lt   | A.Gt -> Some I.Gt
    | A.And -> Some I.And | A.Or -> Some I.Or   | A.Not -> Some I.Not
    | A.Label _ -> None
    | A.Call (f, n)  -> Some (I.Call (f, n))
    | A.SCall f      -> Some (I.SCall f)
    | A.Return -> Some I.Return
    | A.Goto l ->
        begin match Table.find_opt l tbl with
        | Some ln' -> Some (I.Goto (ln' - ln))
        | None -> raise (unbound_label i l) end
    | A.IfGoto l ->
        begin match Table.find_opt l tbl with
        | Some ln' -> Some (I.IfGoto (ln' - ln))
        | None -> raise (unbound_label i l) end in
    match base with
    | Some x    -> [(x, A.inst_annot i)]
    | None      -> []

(* Translate a list of VM instructions to a list of IR instructions. *)
let rec translate_insts tbl ln is =
    match is with
    | []        -> []
    | i :: is'  ->
        let ln_incr =
            match A.base_inst i with
            | A.Label _ -> 0
            | _ -> 1 in
        translate_inst tbl ln i @ translate_insts tbl (ln + ln_incr) is'

(* Check if VM function f does not end with a return. *)
let func_without_return f =
    let body = f.A.body in
    let n_insts = List.length body in
    match fst (List.nth body (n_insts - 1)) with
    | A.Return  -> false
    | _         -> true

(* Translate a VM function to an IR function. *)
let translate_func f =
    if func_without_return f then raise (no_return f f.A.f_name) else
    let tbl = make_table 0 f.A.body in
    let body = Array.of_list (translate_insts tbl 0 f.A.body) in
    I.({ f_name = f.f_name; n_vars = f.n_vars; body = body; annot = f.annot })

(* Translate a bunch of VM programs to an IR program. *)
let translate_progs main_f ps =
    (* One big program. *)
    let p = List.concat ps in
    (* Function bodies and target labels in those functions. *)
    let fs = List.map translate_func p in
    let fns = List.map (fun f -> f.I.f_name) fs in  (* Function names. *)
    let ans = List.map (fun f -> f.I.annot) fs in   (* Function annotations. *)
    let fns_ans = List.combine fns ans in
    (* Checking if the main function is defined. *)
    match List.find_opt ((=) main_f) fns with
    | None      -> raise (no_entry_func main_f)
    | Some _    ->
        (* Checking for duplicate function names. *)
        match MU.find_duplicate fst fns_ans with
        | Some (f, a)   -> raise (dup_func a f)
        | None          ->
            I.{ main_f = main_f ;
                funcs  = MU.hashtbl_of_list (List.combine fns fs) }

(* Translate a VM program to an IR program. *)
let translate_prog main_f p = translate_progs main_f [p]

