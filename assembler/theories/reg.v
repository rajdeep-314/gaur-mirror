(*
    reg.v
*)


(** 
    {1 Register Module}

    This module defines the registers used in the assembler.
*)


From Stdlib Require Import String.
Open Scope string_scope.


(** Type for register , used as [reg.t]. *)
Variant t : Set :=
    | r0  | r1  | r2  | r3
    | r4  | r5  | r6  | r7
    | r8  | r9  | r10 | r11
    | r12 | r13 | r14 | r15.


(** Printer function to print register. *)
Definition to_string (r:reg.t) : string :=
    match r with
    | r0 => "r0" | r1 => "r1" | r2 => "r2" | r3 => "r3"
    | r4 => "r4" | r5 => "r5" | r6 => "r6" | r7 => "r7"
    | r8 => "r8" | r9 => "r9" | r10 => "r10" | r11 => "r11"
    | r12 => "r12" | r13 => "r13" | r14 => "r14" | r15 => "r15"
    end.

