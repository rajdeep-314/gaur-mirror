From Stdlib Require Import ZArith.
From Stdlib Require Import String.
From Stdlib Require Import List.
Open Scope string_scope.

Definition imm :Set := Z.

Inductive reg : Set := 
| r0 | r1 | r2 | r3 | r4 | r5 | r6 | r7 | r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15.  


Inductive t : Set :=
    | lli : reg -> imm -> t 
    | lui : reg -> imm -> t 
    | ld  : reg -> reg -> imm -> t 
    | st  : reg -> reg -> imm -> t  
    | add : reg -> reg -> reg -> t 
    | addi: reg -> reg -> imm -> t 
    | sub : reg -> reg -> reg -> t 
    | and : reg -> reg -> reg -> t 
    | not : reg -> reg -> t 
    | beq : reg -> reg -> imm -> t 
    | blt : reg -> reg -> imm -> t 
    | jalr: reg -> reg -> imm -> t 
    | apc : reg -> imm -> t 
    | j  : imm -> t .


Definition prog := list t.

