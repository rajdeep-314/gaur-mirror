(*
    regfile.mli
*)


open Syntax

type half = int     (* Half-word.     *)
type t              (* Register-file. *)

(* Create an all-zero register-file. *)
val create : unit -> t
(* Read the value of a register. *)
val read : t -> Ast.Reg.t -> half
(* Write a value to a register. *)
val write : t -> Ast.Reg.t -> half -> unit

(* Print the value of a register. *)
val print_reg : t -> Ast.Reg.t -> unit
(* Print the contents of the entire register file. *)
val print : t -> unit

