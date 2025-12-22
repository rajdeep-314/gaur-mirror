(*
    ast.mli
*)


(* Addresses. *)
type addr = int

(* Instructions in the debug mode. *)
type t =
    | Help
    | SetTrace of bool
    | GetTrace
    | Reg of int
    | Regs
    | Print_inst
    | Print_prog
    | Mem of addr * int
    | Step
    | Steps of int
    | Quit

(* String representation of instructions. *)
val to_string : t -> string

(* Print an instruction. *)
val print : t -> unit

