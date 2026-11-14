(*
    vme_live.ml

    Live mode for the VM emulator.
    NOTE: Requires a TTY on both stdin and stdout; exits with an error otherwise.


    WARNING: Needs polishing!
*)


module VE   = Vm.Emulator
module Er   = VE.Err
module P    = VE.State.PP
module I    = VE.Ir
module S    = Vm.Syntax.Emulator
module PE   = S.Parsing.Err
module STbl = Hashtbl.Make (String)
module W    = Types.Word
module U    = Utils.Files

(* Exit codes. *)
let err_code_success        = 0     (* Exited successfully.              *)
let err_code_lexing_parsing = 1     (* Lexing or parsing errors.         *)
let err_code_semantic       = 2     (* Semantic errors during emulation. *)
let err_code_execution      = 3     (* Execution errors.                 *)
let err_code_not_a_tty      = 1     (* Not run on a terminal.            *)
let err_code_interrupted    = 130   (* Aborted with Ctrl-C.              *)

type src_tbl = string STbl.t


let usage_msg = "Usage: vme-live <file1.vm> <file2.vm> ..."
let files = ref []
let anon_fun arg = files := arg :: !files
let speclist = []


(* Saved terminal attributes, so they can be restored on exit. *)
let saved_termios : Unix.terminal_io option ref = ref None

(* ANSI escapes. *)
let esc_clear_and_home = "\027[2J\027[H"
let esc_home           = "\027[H"
let esc_hide_cursor    = "\027[?25l"
let esc_show_cursor    = "\027[?25h"

(* Put stdin into non-canonical, non-echo mode with non-blocking reads.
   ISIG is left enabled so Ctrl-C still generates SIGINT. *)
let set_raw_mode () =
    let t = Unix.tcgetattr Unix.stdin in
    saved_termios := Some t ;
    let t' = { t with
        Unix.c_icanon = false ;
        Unix.c_echo   = false ;
        Unix.c_vmin   = 0 ;
        Unix.c_vtime  = 0 } in
    Unix.tcsetattr Unix.stdin Unix.TCSANOW t'

(* Restore the terminal and show the cursor. Idempotent. *)
let restore_terminal () =
    (match !saved_termios with
    | Some t -> Unix.tcsetattr Unix.stdin Unix.TCSANOW t; saved_termios := None
    | None   -> ()) ;
    print_string esc_show_cursor ; flush stdout

(* Render the screen of state s at the top-left corner of the terminal,
   without clearing. *)
let render s =
    print_string esc_home ;
    print_string (P.screen s) ;
    flush stdout

let clear_and_render s =
    print_string esc_clear_and_home ;
    print_string (P.screen s) ;
    flush stdout


(* How long (in seconds) a pressed key stays pressed with no further input. *)
let key_hold_time = 0.1

(* Bytes of an escape sequence (e.g. arrow keys) to swallow once an ESC is
   seen, so they don't register as keys. *)
let esc_skip = ref 0

(* Poll stdin for pressed keys. Returns the updated state and whether any
   key byte was seen. *)
let poll_keyboard s =
    let ready =
        try (match Unix.select [Unix.stdin] [] [] 0.0 with
             | (fds, _, _) -> fds)
        with Unix.Unix_error (Unix.EINTR, _, _) -> []
    in
    match ready with
    | [] -> (s, false)
    | _ ->
        let b = Bytes.create 64 in
        let n =
            try Unix.read Unix.stdin b 0 64
            with Unix.Unix_error (Unix.EINTR, _, _) -> 0
        in
        let rec loop i st saw =
            if i >= n then (st, saw) else
            let c = Bytes.get b i in
            if !esc_skip > 0 then
                (decr esc_skip; loop (i + 1) st saw)
            else if c = '\x1b' then
                (esc_skip := 2; loop (i + 1) st saw)
            else if c >= '\x20' && c < '\x7f' then
                loop (i + 1) (VE.Emulate.set_key_pressed st c) true
            else
                loop (i + 1) st saw
        in
        loop 0 s false


(* Set when the user presses Ctrl-C. *)
let aborted = ref false

let () =
    (* Require a terminal. *)
    if not (Unix.isatty Unix.stdin && Unix.isatty Unix.stdout) then
        begin
        prerr_endline "vme-live: stdin and stdout must both be terminals";
        exit err_code_not_a_tty
        end ;

    (* Parse command line *)
    Arg.parse speclist anon_fun usage_msg;

    if !files = [] then
        begin
        Arg.usage speclist usage_msg ;
        exit err_code_lexing_parsing
        end ;

    (* Reading all source files into a hash table with keys being paths and
       strings being the content of the corresponding file. *)
    let path_src_tbl : src_tbl =
        STbl.of_seq (List.to_seq (
            List.map (fun p -> (p, U.read_file p)) (List.rev !files))
        ) in

    (* Parse all provided files *)
    let ps, ps_scs =
        try
            List.split (List.map
                S.Parsing.Driver.parse_from_path (List.rev !files))
        with
        | Sys_error s ->
            prerr_endline s; exit err_code_lexing_parsing
        | PE.LexingExn err ->
            let src = STbl.find path_src_tbl (PE.Lexer.fname_from_t err) in
            prerr_endline (PE.Lexer.to_string_with_src err src) ;
            exit err_code_lexing_parsing
        | PE.ParsingExn err ->
            let src = STbl.find path_src_tbl (PE.Parser.fname_from_t err) in
            prerr_endline (PE.Parser.to_string_with_src err src);
            exit err_code_lexing_parsing
    in

    let src_tbl : src_tbl = STbl.of_seq (List.to_seq ps_scs) in
    try
        (* Translate to IR *)
        let ip = VE.Vm2ir.translate_progs "main" ps in

        (* Live execution. *)
        ignore (Sys.signal Sys.sigint (Sys.Signal_handle (fun _ -> aborted := true))) ;
        set_raw_mode () ;
        print_string esc_hide_cursor ;
        print_string esc_clear_and_home ;
        flush stdout ;

        let render_interval = 1. /. 60. in
        let poll_every = 256 in
        let frame_counter = ref 0 in
        let last_render = ref (Unix.gettimeofday ()) in
        let last_key_time = ref 0. in

        let rec loop s =
            incr frame_counter ;
            (* Poll the keyboard every few instructions rather. *)
            let s =
                if !frame_counter mod poll_every = 0 then
                    let now = Unix.gettimeofday () in
                    (* Expire the held key if no input arrived recently. *)
                    let s =
                        match s.VE.State.keyboard with
                        | Some _ when now -. !last_key_time >= key_hold_time ->
                            VE.Emulate.unset_key_pressed s
                        | _ -> s
                    in
                    let s, saw_key = poll_keyboard s in
                    if saw_key then last_key_time := now ;
                    s
                else s
            in
            if !aborted then `Aborted s
            else
                let ins = VE.Emulate.fetch_inst ip s in
                if I.inst_base ins = I.Return && VE.Emulate.n_frames s = 1 then
                    `Terminated s
                else
                    let now = Unix.gettimeofday () in
                    if now -. !last_render >= render_interval then
                        (render s ; last_render := now) ;
                    match VE.Emulate.execute_inst ip s ins with
                    | exception e -> `Failed (e, s)
                    | s' -> loop s'
        in

        let show_final s =
            print_string esc_clear_and_home ;
            print_endline (P.screen s)
        in

        let print_rv s =
            match VE.Emulate.wstack_peek s with
            | Some v -> Printf.printf "Program returned with: %s\n" (W.to_string v)
            | None   -> print_endline "Program returned with: no return value"
        in

        (match loop (VE.Emulate.init_from_prog ip) with
        | `Terminated s ->
            show_final s ;
            print_rv s ;
            restore_terminal () ;
            exit err_code_success
        | `Aborted s ->
            show_final s ;
            print_endline "Interrupted (Ctrl-C)." ;
            restore_terminal () ;
            exit err_code_interrupted
        | `Failed (Er.ExecutionError (a, e), _) ->
            show_final (Er.Execution.state e) ;
            prerr_endline (Er.pp_execution_error src_tbl (a, e)) ;
            restore_terminal () ;
            exit err_code_execution
        | `Failed (exn, _) ->
            restore_terminal () ;
            raise exn)
    with
    | Er.SemanticError (a, e) ->
        restore_terminal () ;
        prerr_endline (Er.pp_semantic_error src_tbl (a, e));
        exit err_code_semantic
    | exn ->
        restore_terminal () ;
        raise exn

