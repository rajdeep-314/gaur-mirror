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


(* Pretty-printers for VM state and related constructs. *)
module PP = struct
    let stack s =
        String.concat ", " (List.map string_of_int s)
end


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

    (* String representation of a call and a list of calls. *)
    let string_of_call (Call (f, args)) =
        f ^ "(" ^ String.concat ", " (List.map string_of_int args) ^ ")"
    let string_of_calls cs = String.concat " -> " (List.map string_of_call cs)


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
    let segment seg s = Syntax.Ast.(
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

    (* Screen in VM state s. *)
    let screen s = String.concat "\n" (lines s)

    (* Screen in VM state s, decorated with borders. *)
    let screen_p s =
        let hor_border = "+" ^ String.make 80 '-' ^ "+" in
        let lines' = List.map (fun x -> "|" ^ x ^ "|") (lines s) in
        String.concat "\n" (hor_border :: lines' @ [hor_border])
end

