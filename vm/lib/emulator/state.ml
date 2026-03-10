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

