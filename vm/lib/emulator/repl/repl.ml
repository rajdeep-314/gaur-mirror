(*
    repl.ml

    VM Emulator REPL.
*)


module E  = Emulate
module I  = State.Inspect
module SP = State.PP
module P  = Parsing.Repl

open Syntax.Repl
open Err.Helper


(*
TODO:
    - Implement mapping IR instructions to AST instructions.
    - Handle lexer and parser exceptions
    - Have a consistent handling of returning from main (exception, or not?)
*)


(*
REPL functions.
These are functions that the main REPL loop calls.

Their signatures are:
    Ast.prog -> State.t -> string * State.t

f p s = (str, s') means that on execution the command corresponding to the
REPL function, for VM program p, in state s, str should be displayed and
the resulting state is s'.
*)


(* REPL functions for show commands. *)

(* Show sequence of function calls. *)
let show_calls p s =
    (I.string_of_calls (I.calls s), s)

(* Show top-most frame's working stack. *)
let show_stack p s =
    match I.wstack_of_top_frame s with
    | Some st   -> ("Stack: " ^ SP.stack st, s)
    | None      -> raise empty_call_stack

(* Show the top (at most) n entries of the current working stack. *)
let show_stack_top n p s =
    match I.wstack_of_top_frame s with
    | Some st   -> ("Stack top: " ^ SP.stack (List.take n st), s)
    | None      -> raise empty_call_stack

(* Show current function. *)
let show_function p s =
    match I.call_of_top_frame s with
    | Some c    -> (I.string_of_call c, s)
    | None      -> raise empty_call_stack

(* Show the current screen. *)
let show_screen p s =
    (I.screen s, s)


(* REPL functions for execute commands. *)

(* Execute a single instruction. *)
let e_step p s =
    let ins = E.fetch_inst p s in
    ("Executed 1 instruction", E.execute_inst p s ins)

(* Execute n instructions. *)
let rec e_steps n counter p s =
    if counter >= n then
        (Printf.sprintf "Executed %d instructions" n, s)
    else
    let ins = E.fetch_inst p s in
    e_steps n (counter + 1) p (E.execute_inst p s ins)

(* Execute till (after the) the current function, which is not main, returns. *)
let e_till_return_non_main p s =
    match I.call_of_top_frame s with
    | Some c -> ("Finished executing " ^ I.string_of_call c,
                 E.e_till_return p s)
    | None -> raise empty_call_stack

(* Execute till (after the) current function returns, unless if it's main. *)
let e_till_return p s =
    if E.n_frames s = 1 then raise e_till_return_from_main
    else e_till_return_non_main p s

(* Execute till (before the) the current function returns. *)
let e_till_before_return p s =
    match I.call_of_top_frame s with
    | Some c -> (I.string_of_call c ^ " ready to return",
                 E.e_till_before_return p s)
    | None -> raise empty_call_stack

(* Execute till the program terminates. *)
let e_till_termination p s =
    let (s', rv) = E.e_till_termination p s in
    ("Execution terminated with return value " ^ string_of_int rv,
     s')

(* Reset to the program's initial state. *)
let reset_f p s = ("Resetting.", E.init_from_prog p)

(* REPL function for exit. *)
let exit_f p s = ("Exiting", s)

(* REPL function for empty input. *)
let empty_f p s = ("", s)


(* REPL function for a command. *)
let repl_func_of_cmd = function
    | Empty                     -> empty_f
    | ShowCalls                 -> show_calls
    | ShowStack                 -> show_stack
    | ShowStackTop n            -> show_stack_top n
    | ShowFunction              -> show_function
    | ShowScreen                -> show_screen
    | ExecuteStep               -> e_step
    | ExecuteSteps n            -> e_steps n 0
    | ExecuteTillReturn         -> e_till_return
    | ExecuteTillBeforeReturn   -> e_till_before_return
    | ExecuteTillTermination    -> e_till_termination
    | Reset                     -> reset_f
    | Exit                      -> exit_f

(* Does an instruction exit the REPL? *)
let exits = function
    | Exit  -> true
    | _     -> false


(* REPL loop on program p and state s. *)
let rec loop p s =
    let () = print_string "@> " in
    let cmd = P.parse_cmd (read_line ()) in
    let repl_f = repl_func_of_cmd cmd in
    let (str, s') = repl_f p s in
    print_endline str ;
    if exits cmd then () else loop p s'

(* Main REPL loop. *)
let main_loop p =
    loop p (E.init_from_prog p)

