(*
    machine.v
*)


(**
    {1 Machine Module}

    This module defines the representation of machine instructions
    and programs in the assembler.
*)

From Stdlib Require Import List String.
Import ListNotations.
Open Scope string_scope.

From assembler Require bits.


(* Represents a 16-bit unit of the binary file - corresponds to an instruction. *)

(** Type definition for 16-bit vector. *)
Definition t:Set := bits.bv 16.

(** Type definition for program. *)
Definition prog:Set := list t.

(* For extraction. *)
Definition prog_erased:Set := list bits.bv_.

