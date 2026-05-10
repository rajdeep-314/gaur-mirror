(*
    repl.ml

    VM Emulator REPL.
*)


module E  = Emulate
module I  = State.Inspect
module SP = State.PP
module P  = Parsing.Repl
module Er = Err

open Ast
open Er.Helper.Repl


(*
TODO:
    - Implement mapping IR instructions to AST instructions.
    - Handle lexer and parser exceptions
    - Have a consistent handling of returning from main (exception, or not?)
*)


let help_msg =
    "Commands:

    help                        :       show list of commands

    show calls                  :       show sequence of function calls so far
    show stack                  :       show current function's working stack
    show stack top n            :       show current working stack's top n
                                            entries
    show function               :       show current function call
    show screen                 :       show current scren
    show segment s              :       show contents of segment s
    show frame                  :       show top-most function frame
    show state                  :       show the entire VM state, except the
                                            screen

    execute step                :       execute one instruction
    execute steps n             :       execute n instructions

    execute till breakpoint s   :       execute till breakpoint with name s
    execute till any breakpoint :       execute till the next breakpoint
    execute till return         :       execute till current function returns
    execute till before return  :       execute till just before current
                                            function returns
    execute till termination    :       execute till the program terminates

    reset                       :       reset emulator state
    exit                        :       exit REPL"


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
    (SP.calls (I.calls s), s)

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
    | Some c    -> (SP.call c, s)
    | None      -> raise empty_call_stack

(* Show the current screen. *)
let show_screen p s = (SP.screen s, s)

(* Show contents of segment seg. *)
let show_segment seg p s = (SP.segment seg s, s)

(* Show top-most frame. *)
let show_frame p s = (SP.top_frame s, s)

(* Show all frames. *)
let show_frames p s = (SP.frames s, s)

(* Show the entire state. *)
let show_state p s = (SP.state s, s)


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

(* Execute till named breakpoint. *)
let e_till_named_bpt bpt_name p s =
    let s' = E.e_till_named_bpt bpt_name p s in
    (Printf.sprintf "Breakpoint %s hit" bpt_name, s')

(* Execute till next breakpoint. *)
let e_till_next_bpt p s =
    let s' = E.e_till_any_bpt p s in
    ("Breakpoint hit", s')

(* Execute till (after the) the current function, which is not main, returns. *)
let e_till_return_non_main p s =
    match I.call_of_top_frame s with
    | Some c -> ("Finished executing " ^ SP.call c,
                 E.e_till_return p s)
    | None -> raise empty_call_stack

(* Execute till (after the) current function returns, unless if it's main. *)
let e_till_return p s =
    if E.n_frames s = 1 then raise e_till_return_from_main
    else e_till_return_non_main p s

(* Execute till (before the) the current function returns. *)
let e_till_before_return p s =
    match I.call_of_top_frame s with
    | Some c -> (SP.call c ^ " ready to return",
                 E.e_till_before_return p s)
    | None -> raise empty_call_stack

(* Execute till the program terminates. *)
let e_till_termination p s =
    let (s', rv) = E.e_till_termination p s in
    let msg = match rv with
    | None      -> "Execution terminated with no return value."
    | Some rv   -> "Execution terminated with return value " ^ string_of_int rv
    in msg, s'

let reset_f p s = ("Resetting.", E.init_from_prog p)
let exit_f p s = ("Exiting", s)
let empty_f p s = ("", s)
let help_f p s = (help_msg, s)


(* REPL function for a command. *)
let repl_func_of_cmd = function
    | Empty                     -> empty_f
    | Help                      -> help_f
    | ShowCalls                 -> show_calls
    | ShowStack                 -> show_stack
    | ShowStackTop n            -> show_stack_top n
    | ShowFunction              -> show_function
    | ShowScreen                -> show_screen
    | ShowSegment s             -> show_segment s
    | ShowFrame                 -> show_frame
    | ShowFrames                -> show_frames
    | ShowState                 -> show_state
    | ExecuteStep               -> e_step
    | ExecuteSteps n            -> e_steps n 0
    | ExecuteTillNamedBpt s     -> e_till_named_bpt s
    | ExecuteTillNextBpt        -> e_till_next_bpt
    | ExecuteTillReturn         -> e_till_return
    | ExecuteTillBeforeReturn   -> e_till_before_return
    | ExecuteTillTermination    -> e_till_termination
    | Reset                     -> reset_f
    | Exit                      -> exit_f

(* Does an instruction exit the REPL? *)
let exits = function
    | Exit  -> true
    | _     -> false


(* REPL loop on program p and state s, with source table src_tbl. *)
let rec loop src_tbl p s =
    print_string "@> " ; flush stdout ;     (* Prompt string. *)
    match P.get_cmd () with                 (* Try to read and parse input. *)
    (* No lexing or parsing errors. *)
    | Ok cmd -> exec_cmd src_tbl p s cmd
    (* End of file while reading for command. *)
    | Error P.EOF -> print_endline "\nExiting"
    (* Lexing or parsing error. *)
    | Error (P.Msg err_str) ->
        print_endline err_str ;
        loop src_tbl p s

and exec_cmd src_tbl p s cmd =
    try
        let str, s' = repl_func_of_cmd cmd p s in
        print_endline str ;
        if exits cmd then () else loop src_tbl p s'
    with
    (* Semantic, execution, or REPL error. *)
    | Er.ReplError _ as e ->
        print_endline (Printexc.to_string e) ;
        loop src_tbl p s
    | Er.ExecutionError (a, e) ->
        print_endline (Er.pp_execution_error src_tbl (a, e)) ;
        loop src_tbl p (Er.Execution.state e)


(* Main REPL loop. *)
let main_loop src_tbl p =
    loop src_tbl p (E.init_from_prog p)

