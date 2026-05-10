(*
    state.ml

    VM state.
*)


type segment = int array        (* Segments are arrays of integers. *)
type stack = int list           (* Stacks are lists of integers. *)

type frame = {
    f_name   : string ;         (* function name *)
    local    : segment ;        (* local segment *)
    argument : segment ;        (* argument segment *)
    pc       : int ;            (* "program-counter" for a function *)
    wstack   : stack            (* working stack *)
}

type t = {
    temp   : segment ;          (* global temporary segment *)
    static : segment ;          (* global static segment *)
    (* TODO: Extend ASCII to UTF. *)
    screen : segment ;          (* 80 x 24 characters screen. ASCII, for now. *)
    frames : frame list         (* function frames -- working stack *)
}


(* Useful helper functions. *)
module Helper = struct
    (* List of list of integers corresponding to screen of state s. *)
    let iss_of_screen s =
        List.init 24
            (fun i -> Array.to_list (Array.sub s.screen (80*i) 80))
end


(* Inspecting VM state. *)
module Inspect = struct

    (* Function call. *)
    type call =
        | Call  of string * int list

    (* Call corresponding to a frame. *)
    let call_of_frame f = Call (f.f_name, Array.to_list f.argument)
    (* List of calls that lead to a VM state. *)
    let calls s = List.map call_of_frame (List.rev s.frames)


    (* Segments and stacks. *)

    let top_frame s =
        match s.frames with
        | []     -> None
        | f :: _ -> Some f

    let ( let* ) = Option.bind

    let call_of_top_frame s     = let* f = top_frame s in Some (call_of_frame f)
    let fname_of_top_frame s    = let* f = top_frame s in Some f.f_name
    let pc_of_top_frame s       = let* f = top_frame s in Some f.pc
    let argument_of_top_frame s = let* f = top_frame s in Some f.argument
    let local_of_top_frame s    = let* f = top_frame s in Some f.local
    let wstack_of_top_frame s   = let* f = top_frame s in Some f.wstack

    (* Contents of segment seg in VM state s. *)
    let segment seg s = Syntax.Emulator.Ast.(
        match seg with
        | Temp      -> Some s.temp
        | Static    -> Some s.static
        | Local     -> local_of_top_frame s
        | Argument  -> argument_of_top_frame s)

    (* Lines corresponding to a VM state's screen. List with 24 80-character
       strings. *)
    let lines s =
        let iss = Helper.iss_of_screen s in
        let to_char i =
            if i >= 32 && i <= 126 then char_of_int i else '?' in
        let css = List.map (List.map to_char) iss in
        List.map
            (fun cs -> String.concat "" (List.map (String.make 1) cs)) css
end


(* Pretty-printers for VM state and related constructs. *)
module PP = struct
    open Inspect

    (* String representation of a call and a list of calls. *)
    let call (Call (f, args)) =
        f ^ "(" ^ String.concat ", " (List.map string_of_int args) ^ ")"

    let calls cs = String.concat " -> " (List.map call cs)

    let stack s =
        String.concat ", " (List.map string_of_int s)

    (* Screen in VM state s, decorated with borders. *)
    let screen s =
        let hor_border = "+" ^ String.make 80 '-' ^ "+" in
        let lines' = List.map (fun x -> "|" ^ x ^ "|") (lines s) in
        String.concat "\n" (hor_border :: lines' @ [hor_border])

    (* Screen in VM state s, without any borders. *)
    let screen_no_border s = String.concat "\n" (lines s)

    (* Contents of a segment, separated by " | ". *)
    let seg xs =
        String.concat " | " (List.map string_of_int (Array.to_list xs))

    (* Contents of segment seg_name. *)
    let segment seg_name s =
        match segment seg_name s with
        | Some xs   -> seg xs
        | None      -> "N/A"

    (* Frame f. *)
    let frame f = Printf.sprintf
        "%s:\nPC: %d\nLocal: %s\nWorking stack: %s"
        (call (call_of_frame f)) f.pc (seg f.local) (stack f.wstack)

    (* Top-most frame. *)
    let top_frame s =
        match top_frame s with
        | Some f    -> frame f
        | None      -> "Call stack empty"

    (* Diagnostic info about a state: call stack and contents of top-most
       frame. *)
    let diagnostic s = Printf.sprintf
        "Call stack:\n%s\n\n%s"
        (String.concat "\n" (List.map call (Inspect.calls s))) (top_frame s)

    let frames s = String.concat "\n\n" (List.map frame (List.rev s.frames))

    let state s = Printf.sprintf
        "Temp: %s\nStatic: %s\n\nFrames:\n%s"
        (segment Temp s) (segment Static s) (frames s)
end

