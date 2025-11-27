(*
    machine.v

    Representation of binary instructions and programs.
*)


From Stdlib Require Import List String.
Import ListNotations.
Open Scope string_scope.

From assembler Require bits.


(* Represents a 16-bit unit of the binary file - corresponds to an instruction. *)
Definition t:Set := bits.bv 16.

Definition prog:Set := list t.

(* For extraction. *)
Definition prog_erased:Set := list bits.bv_.

