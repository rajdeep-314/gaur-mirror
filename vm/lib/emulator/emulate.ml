(*
    emulate.ml

    Emulate the execution of VM programs.
*)


open Ir
open State
open Err.Helper.Execution


(* Make a segment of size n, all values 0. *)
let make_segment n = Array.make n 0

(*let read_segment seg n =*)
(*    let size = Array.length seg in*)
(*    if 0 <= n && n < size then seg.(n)*)
(*    else raise (oob_seg_read seg size n) *)

(* Identity function. *)
let idf = fun x -> x
(* "Unit function" -- does nothing. *)
let uf _ = ()

let init f_name n_lcls n_args = {
    temp = make_segment 16 ;
    static = make_segment 240 ;
    frames = [
    {
        f_name = f_name ;
        local = make_segment n_lcls ;
        argument = make_segment n_args ;
        pc = 0 ;
        wstack = [] ;
    } ]
}


(*
Apply the following functions on a frame:
    - lf    :   update local segment
    - af    :   update argument segment
    - pf    :   new PC from old PC
    - wf    :   new working stack from old working stack
*)
let apply_on_frame lf af pf wf f = {
    f_name = f.f_name ;
    local = (lf f.local; f.local) ;
    argument = (af f.argument; f.argument) ;
    pc = pf f.pc ;
    wstack = wf f.wstack
}

(* Apply these functions on the top-most function frame in the call-stack. *)
let apply_on_top_frame lf af pf wf s = {
    temp = s.temp; static = s.static;
    frames =
    match s.frames with
    | []        -> raise empty_call_stack
    | fr :: frs -> apply_on_frame lf af pf wf fr :: frs
}


(* Applies function f on the working stack of the top-most function and
   returns the new state. *)
(* NOTE:    Should this do PC <- PC + 1? *)
let apply_on_wstack f s = apply_on_top_frame uf uf idf f s


(* (working) stack -> stack functions used in translations. *)

(* Pushing n on the stack. *)
let translate_push_const n =
    let push_c n s = n :: s in apply_on_wstack (push_c n)

(* Apply unary function op to the top-most element. *)
let un_op_f op = function
    | []      -> raise (not_enough_args 1 0)
    | x :: s' -> op x :: s'

(* Apply binary function op to the two top-most elements. *)
let bin_op_f op = function
    | []      -> raise (not_enough_args 2 0)
    | y :: s' ->
        match s' with
        | []        -> raise (not_enough_args 2 1)
        | x :: s''  -> op x y :: s''

(* Binary functions. *)
let add_f = ( + )
let sub_f = ( - )
let and_f = ( land )
let or_f  = ( lor )
(* Unary functions. *)
let neg_f = ( ~- )
let not_f = ( lnot )
(* Comparison functions. *)
let comp_f op = fun x y -> if op x y then -1 else 0
let eq_f x y = comp_f ( = ) x y
let lt_f x y = comp_f ( < ) x y
let gt_f x y = comp_f ( > ) x y


(* state -> state functions used in translations. *)

let translate_add = apply_on_wstack (bin_op_f and_f)
let translate_sub = apply_on_wstack (bin_op_f sub_f)
let translate_and = apply_on_wstack (bin_op_f and_f)
let translate_or  = apply_on_wstack (bin_op_f or_f)
let translate_neg = apply_on_wstack (un_op_f neg_f)
let translate_not = apply_on_wstack (un_op_f not_f)

let execute_inst s = function
    | PushC n -> translate_push_const n s
    | Add -> translate_add s | Sub -> translate_sub s
    | And -> translate_and s | Or  -> translate_or s
    | Neg -> translate_neg s | Not -> translate_not s
    | Goto n -> apply_on_top_frame uf uf (fun p -> p + n) idf s
    | _ -> raise unimplemented


