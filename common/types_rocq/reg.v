(*
    reg.v
*)


(**
    {1 Registers Module}

    This module defines the registers used in the assembler.
*)


From Stdlib Require Import String.
Open Scope string_scope.


(** [t] contains registers [r0] through [r31]. *)
Variant t : Set :=
    | R0  | R1  | R2  | R3
    | R4  | R5  | R6  | R7
    | R8  | R9  | R10 | R11
    | R12 | R13 | R14 | R15
    | R16 | R17 | R18 | R19
    | R20 | R21 | R22 | R23
    | R24 | R25 | R26 | R27
    | R28 | R29 | R30 | R31.


(** [to_string r] is the string representation of register [r]. *)
Definition to_string (r:reg.t) : string :=
    match r with
    | R0 => "r0"   | R1 => "r1"   | R2 => "r2"   | R3 => "r3"
    | R4 => "r4"   | R5 => "r5"   | R6 => "r6"   | R7 => "r7"
    | R8 => "r8"   | R9 => "r9"   | R10 => "r10" | R11 => "r11"
    | R12 => "r12" | R13 => "r13" | R14 => "r14" | R15 => "r15"
    | R16 => "r16" | R17 => "r17" | R18 => "r18" | R19 => "r19"
    | R20 => "r20" | R21 => "r21" | R22 => "r22" | R23 => "r23"
    | R24 => "r24" | R25 => "r25" | R26 => "r26" | R27 => "r27"
    | R28 => "r28" | R29 => "r29" | R30 => "r30" | R31 => "r31"
    end.

