(*
    repl.ml

    AST for VM emulator's REPL.
*)


module A = Syntax.Emulator.Ast


type t =
    (* No input. *)
    | Empty
    (* Help. *)
    | Help
    (* Show commands. *)
    | ShowCalls
    | ShowStack
    | ShowStackTop  of int
    | ShowFunction
    | ShowScreen
    (*| ShowSegment   of A.segment*)
    (*| ShowFrame*)
    (*| ShowState*)
    (* Execute commands. *)
    | ExecuteStep
    | ExecuteSteps  of int
    | ExecuteTillReturn
    | ExecuteTillBeforeReturn
    | ExecuteTillTermination
    (* Meta commands. *)
    (*| PrintProgram*)
    (*| PrintFunction*)
    | Reset
    | Exit


