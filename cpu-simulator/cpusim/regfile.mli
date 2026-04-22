(*
    regfile.mli
*)


open Syntax

type word = int     (* word.     *)
type t              (* Register-file. *)

(* Create an all-zero register-file. *)
val create : unit -> t
(* Read the value of a register. *)
val read : t -> Ast.reg -> word
(* Write a value to a register. *)
val write : t -> Ast.reg -> word -> unit

(* Print the value of a register. *)
val print_reg : t -> Ast.reg -> unit
(* Print the contents of the entire register file. *)
val print : t -> unit

