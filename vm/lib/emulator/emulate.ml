(*
    emulate.ml

    Emulate the execution of VM programs.
*)


module A = Syntax.Emulator.Ast

open Ir
open State
open Err.Helper.Execution
open Utils.Misc


let ( let* ) = Result.bind
let ( let+ ) = Option.bind


(* Some functions that will be used frequently in the emulation. *)

(* Make a segment of size n, all values 0. *)
let make_segment n = Array.make n 0

(* Make a screen segment of size n, all values 32 (space). *)
let make_screen n = Array.make n 32

(* Read the n'th index of segment seg. Return [Some x] if index is in bounds
   and corresponds to [x], else [None]. *)
let read_segment seg n =
    let size = Array.length seg in
    if 0 <= n && n < size then Some seg.(n) else None

(* Write the n'th index of segment seg with value v. Returns true for
   successful writes, else false. *)
let write_segment seg n v =
    let size = Array.length seg in
    if 0 <= n && n < size then (seg.(n) <- v; true)
    else false

(* In state s, write value y at offset x of screen and return new state. *)
let write_to_screen s x y =
    s.screen.(x) <- y; s

(* Get instruction number n from function f, whose body is body.
   Previous checks in the pipeline ensure that this does not cause an error. *)
let get_inst f body n = body.(n)

(* Number of frames in VM state s. *)
let n_frames s = List.length s.frames


(* Identity function. *)
let idf = fun x -> x
(* "Unit function" -- does nothing. *)
let uf _ = ()


(* Functions to inspect VM state. *)
let top_frame s = List.hd s.frames

(* Tuple of the top frame, and the other frames in the call stack. *)
let top_and_rest_frames s = List.hd s.frames, List.drop 1 s.frames

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
    screen = make_screen (80 * 24) ;
    frames = [
    {
        f_name = f_name ;
        local = make_segment n_lcls ;
        argument = make_segment n_args ;
        pc = 0 ;
        wstack = [] ;
    } ]
}

(* Initial VM state from program p.
   NOTE: main_f will always be found in the function table, as if not, the VM
   to IR phase would have thrown an error. *)
let init_from_prog p =
    let p = p in
    let fn = Hashtbl.find p.funcs p.main_f in
        init p.main_f fn.n_vars 0

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

(* Similar to apply_on_frame, but the working stack function can result in
   errors. *)
let apply_on_frame_result lf af pf wf f =
    let* s = wf f.wstack in Ok
    { f with
        local = (lf f.local ; f.local) ;
        argument = (af f.argument ; f.argument) ;
        pc = pf f.pc ;
        wstack = s }

(* Apply these functions on the top-most function frame in the call-stack. *)
let apply_on_top_frame lf af pf wf s = {
    s with frames =
        let fr, frs = top_and_rest_frames s in
        apply_on_frame lf af pf wf fr :: frs
}

(* Result variant of apply_on_top_frame. *)
let apply_on_top_frame_result lf af pf wf s =
    let fr, frs = top_and_rest_frames s in
    let* fr' = apply_on_frame_result lf af pf wf fr in
    Ok { s with frames = fr' :: frs }


(* Applies function f on the working stack of the top-most function and
   returns the new state. Increments PC by 1. *)
let apply_on_wstack f s = apply_on_top_frame uf uf (fun x -> x + 1) f s

(* Result variable of apply_on_wstack. *)
let apply_on_wstack_result f s = apply_on_top_frame_result uf uf ((+) 1) f s

(* Peek at the working stack's top. *)
let wstack_peek s =
    match (top_frame s).wstack with
    | []        -> None
    | x :: _    -> Some x

(* Update VM state s by adding a new frame to the call-stack, for
   function name f with n_vars local variables and argument list xs. *)
let push_frame f n_vars xs s = {
    s with
    frames = {
        f_name = f;
        local = make_segment n_vars;
        argument = Array.of_list xs;
        pc = 0; wstack = []
    } :: s.frames
}


(* Evaluating expressions and conditional expressions on VM state. *)

let binop_f = function
    | B_add -> ( + ) | B_sub -> ( - )
    | B_mul -> ( * ) | B_div -> ( / )

let relop_f = function
    | R_eq -> ( = )  | R_lt -> ( < )  | R_gt -> ( > )
    | R_le -> ( <= ) | R_ge -> ( >= ) | R_ne -> ( <> )

let boolop_f = function
    | Bl_and -> ( && ) | Bl_or -> ( || )

let rec eval_exp st e =
    let f = top_frame st in
    let ee = eval_exp st in
    match e with
    | Const n               -> Some n
    | StackOffset n         -> List.nth_opt f.wstack n
    | SegmentOffset (s, n)  -> List.nth_opt (Array.to_list (segment s st)) n
    | Neg e                 -> let+ x = ee e in Some (~-x)
    | BinOp (o, e1, e2)     ->
        let+ x = ee e1 in let+ y = ee e2 in
        Some (binop_f o x y)

let rec eval_cond_exp st e =
    let ee = eval_exp st in
    let ec = eval_cond_exp st in
    match e with
    | True  -> Some true
    | False -> Some false
    | Not c -> let+ x = ec c in Some (not x)
    | Comp (o, e1, e2) ->
        let+ x = ee e1 in let+ y = ee e2 in
        Some (relop_f o x y)
    | BoolOp (o, c1, c2) ->
        let+ x = ec c1 in let+ y = ec c2 in
        Some (boolop_f o x y)


(* (working) stack -> stack functions used in emulation. *)

(* Pushing n on stack s. *)
let push_const n = fun s -> n :: s


(*
Not all functions can be applied on all stacks. For example, functions that
correspond to some binary operator require at least two values to be present on
the stack. For that reason, the actual type of these functions is not stack ->
stack, but stack -> (stack, int * int) result.

- Ok s denotes that the function was applied successfully and resulted in the
  stack s.
- Error (x, y) denotes that the function could not be applied successfully due
  to fewer arguments available than required. It expected at least x arguments,
  but only got y.
*)

(* Apply unary function op to the top-most element. *)
let un_op_f op = function
    | []      -> Error (1, 0)
    | x :: xs -> Ok (op x :: xs)

(* Apply binary function op to the two top-most elements. *)
let bin_op_f op = function
    | []      -> Error (2, 0)
    | y :: s' ->
        match s' with
        | []        -> Error (2, 1)
        | x :: s''  -> Ok (op x y :: s'')

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
    | []        -> Error (1, 0)
    | _ :: xs   -> Ok xs


(* state -> (state, int * int) result functions used in emulation. *)

let exec_add_res = apply_on_wstack_result (bin_op_f add_f)
let exec_sub_res = apply_on_wstack_result (bin_op_f sub_f)
let exec_and_res = apply_on_wstack_result (bin_op_f and_f)
let exec_or_res  = apply_on_wstack_result (bin_op_f or_f)
let exec_neg_res = apply_on_wstack_result (un_op_f neg_f)
let exec_not_res = apply_on_wstack_result (un_op_f not_f)
let exec_eq_res  = apply_on_wstack_result (bin_op_f eq_f)
let exec_lt_res  = apply_on_wstack_result (bin_op_f lt_f)
let exec_gt_res  = apply_on_wstack_result (bin_op_f gt_f)


(*
Execution functions are of the type
        def_inst -> t (and some other arguments, possibly) -> t.

They are the ones that raise any exceptions, as they have the values required
to raise exceptions: the instruction involved, and the faulty state.
*)


(* For [res_f : t -> (t, int * int) result], [exec_of_res res_f] gives a
   function of type [def_inst -> t -> t] (execution functions), which raises
   exception [not_enough_vals] when necessary. *)
let exec_of_res res_f = fun i s ->
    match res_f s with
    | Ok s         -> s
    | Error (x, y) -> raise (not_enough_vals i s x y)

(* Execution functions for binary, unary, and comparison instructions. *)
let exec_add = exec_of_res exec_add_res
let exec_sub = exec_of_res exec_sub_res
let exec_and = exec_of_res exec_and_res
let exec_or  = exec_of_res exec_or_res
let exec_neg = exec_of_res exec_neg_res
let exec_not = exec_of_res exec_not_res
let exec_eq  = exec_of_res exec_eq_res
let exec_lt  = exec_of_res exec_lt_res
let exec_gt  = exec_of_res exec_gt_res


(* Execute `if-goto n` on VM state s. *)
let exec_if_goto i s n =
    match wstack_peek s with
    | None -> raise (not_enough_vals i s 1 0)
    | Some x ->
        let pf = if x = -1 then (+) n else (+) 1 in
        match apply_on_top_frame_result uf uf pf pop_wstack s with
        | Ok s         -> s
        | Error (x, y) -> raise (not_enough_vals i s x y)

(*
Execute a function return. In case there is only one function frame on the
call-stack, raises an exception to denote the termination of execution.
*)
let exec_return i s = {
    s with frames =
        let f, fs = top_and_rest_frames s in
        match wstack_peek s with
        | None -> raise (no_ret_val i s)
        | Some ret_val ->
            match fs with
            | []        -> raise (execution_terminated i s ret_val)
            | f' :: fs' ->
                apply_on_frame uf uf idf (push_const ret_val) f' :: fs'
}

(* Execute `call f n_args` on VM state s with IR program p. *)
let exec_call i s p f n_args =
    (* Getting arguments from top-most frame. *)
    let ws = wstack_of_top_frame s in
    let split_opt = list_split_opt ws n_args in
    match split_opt with
    | None          -> raise (not_enough_vals i s n_args (List.length ws))
    | Some (xs, ys) ->
        (* State after popping n_args many values from the top-most frame's
           working stack. *)
        let s' = apply_on_top_frame uf uf (fun pc -> pc + 1) (fun _ -> ys) s in
        (* Adding the new function's frame to call stack. *)
        match Hashtbl.find_opt p.funcs f with
        | Some fn   -> push_frame f fn.n_vars (List.rev xs) s'
        | None      -> raise (unbound_func i s f)

(* Execute push seg x on VM state s. *)
let exec_push i s seg x =
    let xs = segment seg s in
    match read_segment xs x with
    | None   -> raise (oob_seg_read i s seg (Array.length xs) x)
    | Some v -> apply_on_wstack (push_const v) s

(* Execute pop seg i on VM state s. *)
let exec_pop i s seg x =
    let ws = wstack_of_top_frame s in
    match list_head_split_opt ws with
    | None          -> raise (not_enough_vals i s 1 0)
    | Some (y, ys)  ->
        let s' = apply_on_wstack (fun _ -> ys) s in
        let ys = segment seg s' in
        if not (write_segment ys x y) then
            raise (oob_seg_write i s seg (Array.length ys) x)
        else s'

(* Execute the putchar system call. *)
let exec_putchar i s =
    let ws = wstack_of_top_frame s in
    match list_head_split_opt ws with
    | Some (y, ys) ->
        begin match list_head_split_opt ys with
        | Some (x, xs) ->
            let s' = apply_on_wstack (fun _ -> 0 :: xs) s in
            write_to_screen s' x y
        | None -> raise (not_enough_vals i s 2 1) end
    | None -> raise (not_enough_vals i s 2 0)

(* Execute a system call. *)
let exec_syscall i s = function
    | A.PutChar -> exec_putchar i s


(* Execute instruction i on state s, p being the IR program. *)
let execute_inst p s i =
    match inst_base i with
    | Push (seg, x) -> exec_push i s seg x
    | Pop (seg, x)  -> exec_pop i s seg x
    | PushC n   -> apply_on_wstack (push_const n) s
    | Add       -> exec_add i s | Sub -> exec_sub i s | And -> exec_and i s
    | Or        -> exec_or i s  | Neg -> exec_neg i s | Not -> exec_not i s
    | Eq        -> exec_eq i s  | Lt  -> exec_lt i s  | Gt  -> exec_gt i s
    | Goto n    -> apply_on_top_frame uf uf ((+) n) idf s
    | IfGoto n  -> exec_if_goto i s n
    | Return    -> exec_return i s
    | Call (f, n_args)  -> exec_call i s p f n_args
    | SCall f           -> exec_syscall i s f
    | LineBpt _ | CondBpt _ -> apply_on_top_frame uf uf ((+) 1) idf s

(* Fetches the next instruction to execute from program p and VM state s. *)
let fetch_inst p s =
    let f = fname_of_top_frame s in
    let pc = pc_of_top_frame s in
    let fn = Hashtbl.find p.funcs f in
    get_inst f fn.body pc



(*
Conditional execution functions.

The function names are prefixed by e_till (roughly), to denote conditional
execution functions.
*)


(* e_till_condition p is_cond s executes VM program p on VM state s till the
   tuple of fetched instruction in a state and that state satisfy the predicate
   is_cond. *)
let rec e_till_condition p is_cond s =
    let ins = fetch_inst p s in
    if is_cond (ins, s) then s
    else e_till_condition p is_cond (execute_inst p s ins)

(* Execute till a breakpoint is encountered that breaks execution, and it's
   name satisfies name_cond. *)
let e_till_bpt name_cond p s =
    let cond_func (i, s) =
        match inst_base i with
        | LineBpt (x, Some _)   -> raise (unimplemented i s)
        | LineBpt (x, None)     -> name_cond x
        | CondBpt (x, c)        ->
            begin match eval_cond_exp s c with
            | None   -> raise (invalid_bpt_exp i s)
            | Some b -> name_cond x && b end
        | _ -> false in
    e_till_condition p cond_func s

let e_till_named_bpt bpt_name = e_till_bpt ((=) bpt_name)
let e_till_any_bpt = e_till_bpt (Fun.const true)

(* Execute program p on state s till it returns from main.
   Returns final state and return value. *)
let e_till_termination p s =
    let s' = e_till_condition p
        (fun (i, s) -> inst_base i = Return && n_frames s = 1) s in
    (s', wstack_peek s')

(* Excute program p on state s till it returns from the current function.
   Returns the state after this return. *)
let e_till_return p s =
    e_till_condition p (fun (_, s') -> n_frames s' < n_frames s) s

(* Excute program p on state s till it returns from the current function.
   Returns the state just before this return. *)
let e_till_before_return p s =
    e_till_condition p
        (fun (i, s') -> inst_base i = Return && n_frames s' = n_frames s) s

(* Executes program p till termination. *)
let e_prog_till_termination p =
    e_till_termination p (init_from_prog p)

