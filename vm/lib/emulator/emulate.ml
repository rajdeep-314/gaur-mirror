(*
    emulate.ml

    Emulate the execution of VM programs.
*)


module A = Syntax.Ast

open Ir
open State
open Err.Helper.Execution
open Utils.Misc


(* Some functions that will be used frequently in the emulation. *)

(* Make a segment of size n, all values 0. *)
let make_segment n = Array.make n 0

(* Read the n'th index from segment seg_name, with contents seg. *)
let read_segment seg_name seg n =
    let size = Array.length seg in
    if 0 <= n && n < size then seg.(n)
    else raise (oob_seg_read seg_name size n)

(* Write value v in the n'th index of segment seg_name, with contents seg. *)
let write_segment seg_name seg n v =
    let size = Array.length seg in
    if 0 <= n && n < size then seg.(n) <- v
    else raise (oob_seg_write seg_name size n)

(* Get instruction number n from function f, whose body is body. *)
let get_inst f body n =
    let size = Array.length body in
    if 0 <= n && n < size then body.(n)
    else raise (oob_pc f size n)

(* Number of frames in VM state s. *)
let n_frames s = List.length s.frames


(* Identity function. *)
let idf = fun x -> x
(* "Unit function" -- does nothing. *)
let uf _ = ()


(* Functions to inspect VM state. *)
let top_frame s =
    match s.frames with
    | []     -> raise empty_call_stack
    | f :: _ -> f

let fname_of_top_frame s    = (top_frame s).f_name
let pc_of_top_frame s       = (top_frame s).pc
let argument_of_top_frame s = (top_frame s).argument
let local_of_top_frame s    = (top_frame s).local
let wstack_of_top_frame s   = (top_frame s).wstack

(* Get segment seg from VM state s. *)
let segment seg s =
    match seg with
    | A.Temp     -> s.temp
    | A.Static   -> s.static
    | A.Local    -> local_of_top_frame s
    | A.Argument -> argument_of_top_frame s


(* Initial VM state. *)
let init f_name n_lcls n_args = {
    temp   = make_segment 16 ;
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

(* Pop the top-most n values from a working stack. Returns list of values
   popped and remaining stack content. *)
let pop_n n s =
    match list_split_opt s n with
    | None   -> raise (not_enough_vals n (List.length s))
    | Some p -> p

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
   returns the new state. Increments PC by 1. *)
let apply_on_wstack f s = apply_on_top_frame uf uf (fun x -> x + 1) f s

(* Top-most entry of the top-most frame's working stack. *)
let wstack_peek s =
    match (top_frame s).wstack with
    | []      -> raise (not_enough_vals 1 0)
    | x :: xs -> x

(* Update VM state s by adding a new frame to the call-stack, for
   function name f with n_vars local variables and argument list xs. *)
let push_frame f n_vars xs s = {
    temp = s.temp; static = s.static;
    frames = {
        f_name = f;
        local = make_segment n_vars;
        argument = Array.of_list xs;
        pc = 0; wstack = []
    } :: s.frames
}


(* (working) stack -> stack functions used in emulation. *)

(* Pushing n on the stack. *)
let push_const n = fun s -> n :: s

(* Apply unary function op to the top-most element. *)
let un_op_f op = function
    | []      -> raise (not_enough_vals 1 0)
    | x :: s' -> op x :: s'

(* Apply binary function op to the two top-most elements. *)
let bin_op_f op = function
    | []      -> raise (not_enough_vals 2 0)
    | y :: s' ->
        match s' with
        | []        -> raise (not_enough_vals 2 1)
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

(* Pop the top-most entry of the working stack and return the rest of it. *)
let pop_wstack = function
    | []        -> raise (not_enough_vals 1 0)
    | _ :: xs   -> xs


(* state -> state functions used in emulation. *)

let exec_add = apply_on_wstack (bin_op_f add_f)
let exec_sub = apply_on_wstack (bin_op_f sub_f)
let exec_and = apply_on_wstack (bin_op_f and_f)
let exec_or  = apply_on_wstack (bin_op_f or_f)
let exec_neg = apply_on_wstack (un_op_f neg_f)
let exec_not = apply_on_wstack (un_op_f not_f)
let exec_eq  = apply_on_wstack (bin_op_f eq_f)
let exec_lt  = apply_on_wstack (bin_op_f lt_f)
let exec_gt  = apply_on_wstack (bin_op_f gt_f)

(* Execute `if-goto n` on VM state s. *)
let exec_if_goto n s =
    let x = wstack_peek s in
    let pf =
        if x = -1 then fun p -> p + n else fun p -> p + 1 in
    apply_on_top_frame uf uf pf pop_wstack s

(*
Execute a function return. In case there is only one function frame on the
call-stack, raises an exception to denote the termination of execution.
*)
let exec_return s = {
    temp = s.temp; static = s.static;
    frames =
    match s.frames with
    | []        -> raise empty_call_stack
    | f :: fs   ->
        let ret_val = wstack_peek s in
        match fs with
        | []        -> raise (execution_terminated ret_val)
        | f' :: fs' ->
            apply_on_frame uf uf idf (push_const ret_val) f' :: fs'
}

(* Execute `call f n_args` on VM state s with IR program p. *)
let exec_call p f n_args s =
    (* Getting arguments from top-most frame. *)
    let ws = wstack_of_top_frame s in
    let split_opt = list_split_opt ws n_args in
    match split_opt with
    | None          -> raise (not_enough_vals n_args (List.length ws))
    | Some (xs, ys) ->
        (* State after popping n_args many values from the top-most frame's
           working stack. *)
        let s' = apply_on_top_frame uf uf (fun pc -> pc + 1) (fun _ -> ys) s in
        (* Adding the new function's frame to call stack. *)
        match Hashtbl.find_opt p f with
        | Some fn   -> push_frame f fn.n_vars (List.rev xs) s'
        | None      -> raise (unbound_func f)

(* Execute push seg i on VM state s. *)
let exec_push s seg i =
    let xs = segment seg s in
    let v = read_segment seg xs i in
    apply_on_wstack (push_const v) s

(* Execute pop seg i on VM state s. *)
let exec_pop s seg i =
    let ws = wstack_of_top_frame s in
    match list_head_split_opt ws with
    | None          -> raise (not_enough_vals 1 0)
    | Some (x, xs)  ->
        let s' = apply_on_wstack (fun _ -> xs) s in
        let xs = segment seg s' in
        let () = write_segment seg xs i x in s'


(* Execute instruction i on state s, p being the IR program. *)
let execute_inst p s i =
    match i with
    | Push (seg, i) -> exec_push s seg i
    | Pop (seg, i) -> exec_pop s seg i
    | PushC n   -> apply_on_wstack (push_const n) s
    | Add       -> exec_add s | Sub -> exec_sub s | And -> exec_and s 
    | Or        -> exec_or s  | Neg -> exec_neg s | Not -> exec_not s
    | Eq        -> exec_eq s  | Lt  -> exec_lt s  | Gt  -> exec_gt s
    | Goto n    -> apply_on_top_frame uf uf (fun p -> p + n) idf s
    | IfGoto n  -> exec_if_goto n s
    | Return    -> exec_return s
    | Call (f, n_args) -> exec_call p f n_args s

(* Fetches the next instruction to execute from program p and VM state s. *)
let fetch_inst p s =
    let f = fname_of_top_frame s in
    let pc = pc_of_top_frame s in
    match Hashtbl.find_opt p f with
    | Some fn   -> get_inst f fn.body pc
    | None      -> raise (unbound_func f)



(*
Execution functions.
The function names are prefixed by e_, to denote execution functions.
*)


(* e_till_condition p i_cond s_cond s executes VM program p on VM state s till
   a state is reached where s_cond is true, and i_cond is true for the fetched
   instruction in that state. *)
let rec e_till_condition p i_cond s_cond s =
    let ins = fetch_inst p s in
    if i_cond ins && s_cond s then s
    else e_till_condition p i_cond s_cond (execute_inst p s ins)


(* Execute program p on state s till it returns from main.
   Returns final return value. *)
let e_till_termination p s =
    let s' = e_till_condition p
        (fun  i -> i = Return) (fun s -> n_frames s = 1) s in
    wstack_peek s'

(* Excute program p on state s till it returns from the current function.
   Returns the state after this return. *)
let e_till_return p s =
    e_till_condition p
        (fun _ -> true) (fun s' -> n_frames s' < n_frames s) s

(* Excute program p on state s till it returns from the current function.
   Returns the state just before this return. *)
let e_till_before_return p s =
    e_till_condition p
        (fun i -> i = Return) (fun s' -> n_frames s' = n_frames s) s


(* Executes program p till termination, with main_f being the main function. *)
let e_prog_till_termination main_f p =
    match Hashtbl.find_opt p main_f with
    | Some fn   -> e_till_termination p (init main_f fn.n_vars 0)
    | None      -> raise (unbound_func main_f)

