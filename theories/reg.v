(*
    reg.v

    Type for registers.
*)


From Stdlib Require Import String.
Open Scope string_scope.


(* As both ast and isa has re*)
Variant t : Set :=
    | r0 | r1 | r2 | r3
    | r4 | r5 | r6 | r7
    | r8 | r9 | r10 | r11
    | r12 | r13 | r14 | r15.


Definition to_string (r:reg.t) : string :=
    match r with
    | r0 => "r0" | r1 => "r1" | r2 => "r2" | r3 => "r3"
    | r4 => "r4" | r5 => "r5" | r6 => "r6" | r7 => "r7"
    | r8 => "r8" | r9 => "r9" | r10 => "r10" | r11 => "r11"
    | r12 => "r12" | r13 => "r13" | r14 => "r14" | r15 => "r15"
    end.

