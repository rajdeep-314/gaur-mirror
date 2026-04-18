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
    | r12 | r13 | r14 | r15
    | r16 | r17 | r18 | r19
    | r20 | r21 | r22 | r23
    | r24 | r25 | r26 | r27
    | r28 | r29 | r30 | r31.


(** Printer function to print register. *)
Definition to_string (r:reg.t) : string :=
    match r with
    | r0 => "r0" | r1 => "r1" | r2 => "r2" | r3 => "r3"
    | r4 => "r4" | r5 => "r5" | r6 => "r6" | r7 => "r7"
    | r8 => "r8" | r9 => "r9" | r10 => "r10" | r11 => "r11"
    | r12 => "r12" | r13 => "r13" | r14 => "r14" | r15 => "r15"
    | r16 => "r16" | r17 => "r17" | r18 => "r18" | r19 => "r19"
    | r20 => "r20" | r21 => "r21" | r22 => "r22" | r23 => "r23"
    | r24 => "r24" | r25 => "r25" | r26 => "r26" | r27 => "r27"
    | r28 => "r28" | r29 => "r29" | r30 => "r30" | r31 => "r31"
    end.

