From Stdlib Require Import ZArith.
From Stdlib Require Import String.
From Stdlib Require Import List.
Open Scope string_scope.

From assembler Require bits.
From assembler Require reg.
Import reg.
(* Definition imm :Set := Z. *)


Inductive t : Set :=
    | lli : reg -> bits.bv 8 -> t 
    | lui : reg -> bits.bv 8  -> t 
    | ld  : reg -> reg -> bits.bv 4 -> t 
    | st  : reg -> reg -> bits.bv 4 -> t  
    | add : reg -> reg -> reg -> t 
    | addi: reg -> reg -> bits.bv 4 -> t 
    | sub : reg -> reg -> reg -> t 
    | and : reg -> reg -> reg -> t 
    | not : reg -> reg -> t 
    | beq : reg -> reg -> bits.bv 4 -> t 
    | blt : reg -> reg -> bits.bv 4 -> t 
    | jalr: reg -> reg -> bits.bv 4 -> t 
    | apc : reg -> bits.bv 8 -> t 
    | j  : bits.bv 12 -> t .


Definition prog := list t.

