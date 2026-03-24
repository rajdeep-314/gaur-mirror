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
    frames : frame list         (* function frames -- working stack *)
}


(* Pretty-printers for VM state and related constructs. *)
module PP = struct
    let stack s =
        String.concat ", " (List.map string_of_int s)
end


(* Inspecting VM state. *)
module Inspect = struct

    (* Function call. *)
    type call =
        | Call  of string * int list

    (* Call corresponding to a frame. *)
    let call_of_frame f = Call (f.f_name, Array.to_list f.argument)
    (* List of calls that lead to a VM state. *)
    let calls s = List.map call_of_frame s.frames

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
end

