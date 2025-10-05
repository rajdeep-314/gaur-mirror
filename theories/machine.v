(* Representation for binary files, etc. *)

From Stdlib Require Import List String.
Import ListNotations.
Open Scope string_scope.

From assembler Require bits.


(* Represents a 16-bit unit of the binary file - corresponds to an instruction. *)
Definition t : Set := bits.bv 16.

Definition prog : Set := list t.


(* String representation of a machine instruction. *)
Definition inst_to_string : t -> string := bits.bv_to_string 16.

(* String representation of a machine program. *)
Fixpoint prog_to_string (p:prog) : string :=
    match p with
    | [] => ""
    | inst :: p' => inst_to_string inst ++ "\n" ++ prog_to_string p'
    end.

