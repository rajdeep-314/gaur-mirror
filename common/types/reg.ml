(*
    reg.ml
*)


(** Registers. *)
type t =
    | R0  | R1  | R2  | R3
    | R4  | R5  | R6  | R7
    | R8  | R9  | R10 | R11
    | R12 | R13 | R14 | R15
    | R16 | R17 | R18 | R19
    | R20 | R21 | R22 | R23
    | R24 | R25 | R26 | R27
    | R28 | R29 | R30 | R31


(** Functions for string representations of registers. *)
module Printers = struct
    (** String corresponding to a register. *)
    let reg = function
        | R0    -> "r0"  | R1    -> "r1"  | R2    -> "r2"  | R3    -> "r3"
        | R4    -> "r4"  | R5    -> "r5"  | R6    -> "r6"  | R7    -> "r7"
        | R8    -> "r8"  | R9    -> "r9"  | R10   -> "r10" | R11   -> "r11"
        | R12   -> "r12" | R13   -> "r13" | R14   -> "r14" | R15   -> "r15"
        | R16   -> "r16" | R17   -> "r17" | R18   -> "r18" | R19   -> "r19"
        | R20   -> "r20" | R21   -> "r21" | R22   -> "r22" | R23   -> "r23"
        | R24   -> "r24" | R25   -> "r25" | R26   -> "r26" | R27   -> "r27"
        | R28   -> "r28" | R29   -> "r29" | R30   -> "r30" | R31   -> "r31"
end

module Helper = struct
    let r0 = R0
    let r1 = R1
    let r2 = R2
    let r3 = R3
    let r4 = R4
    let r5 = R5
    let r6 = R6
    let r7 = R7
    let r8 = R8
    let r9 = R9
    let r10 = R10
    let r11 = R11
    let r12 = R12
    let r13 = R13
    let r14 = R14
    let r15 = R15
    let r16 = R16
    let r17 = R17
    let r18 = R18
    let r19 = R19
    let r20 = R20
    let r21 = R21
    let r22 = R22
    let r23 = R23
    let r24 = R24
    let r25 = R25
    let r26 = R26
    let r27 = R27
    let r28 = R28
    let r29 = R29
    let r30 = R30
    let r31 = R31
end

