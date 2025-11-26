(* Some helper functions for the AST, for testing. *)

open Types

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

let lli r i = Lli (r, i)
let lui r i = Lui (r, i)
let ld r1 r2 i = Ld (r1, r2, i)
let st r1 r2 i = St (r1, r2, i)
let add r1 r2 r3 = Add (r1, r2, r3)
let addi r1 r2 i = Addi (r1, r2, i)
let sub r1 r2 r3 = Sub (r1, r2, r3)
let and_ r1 r2 r3 = And (r1, r2, r3)
let not r1 r2 = Not (r1, r2)
let beq r1 r2 l = Beq (r1, r2, l)
let blt r1 r2 l = Blt (r1, r2, l)
let jalr r1 r2 i = Jalr (r1, r2, i)
let apc r i = Apc (r, i)
let j x = J x
let label x = Label x
let spin = Spin

