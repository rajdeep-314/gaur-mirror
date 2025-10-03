From Stdlib Require Import ZArith.
From Stdlib Require Import String.
From Stdlib Require Import List.
Import ListNotations.
Open Scope string_scope.

From assembler Require bits.
From assembler Require isa.
From assembler Require reg.
Import reg.

Definition opcode_of (inst:isa.t):bits.bv 4:=
    match inst with
    | isa.lli  _ _    => bits.reverse 4 (bits.int_to_bv_n 4 0)
    | isa.lui  _ _    => bits.reverse 4 (bits.int_to_bv_n 4 1)
    | isa.ld   _ _ _  => bits.reverse 4 (bits.int_to_bv_n 4 2)
    | isa.st   _ _ _  => bits.reverse 4 (bits.int_to_bv_n 4 3)
    | isa.add  _ _ _  => bits.reverse 4 (bits.int_to_bv_n 4 4)
    | isa.addi _ _ _  => bits.reverse 4 (bits.int_to_bv_n 4 5)
    | isa.sub  _ _ _  => bits.reverse 4 (bits.int_to_bv_n 4 6)
    | isa.and  _ _ _  => bits.reverse 4 (bits.int_to_bv_n 4 7)
    | isa.not  _ _    => bits.reverse 4 (bits.int_to_bv_n 4 8)
    | isa.beq  _ _ _  => bits.reverse 4 (bits.int_to_bv_n 4 9)
    | isa.blt  _ _ _  => bits.reverse 4 (bits.int_to_bv_n 4 10)
    | isa.jalr _ _ _  => bits.reverse 4 (bits.int_to_bv_n 4 11)
    | isa.apc  _ _    => bits.reverse 4 (bits.int_to_bv_n 4 12)
    | isa.j    _      => bits.reverse 4 (bits.int_to_bv_n 4 13)
    end.

Definition reg_to_bv (r:reg):bits.bv 4:=
    match r with 
    | r0 => bits.reverse 4 (bits.int_to_bv_n 4 0)
    | r1 => bits.reverse 4 (bits.int_to_bv_n 4 1)
    | r2 => bits.reverse 4 (bits.int_to_bv_n 4 2)
    | r3 => bits.reverse 4 (bits.int_to_bv_n 4 3)
    | r4 => bits.reverse 4 (bits.int_to_bv_n 4 4)
    | r5 => bits.reverse 4 (bits.int_to_bv_n 4 5)
    | r6 => bits.reverse 4 (bits.int_to_bv_n 4 6)
    | r7 => bits.reverse 4 (bits.int_to_bv_n 4 7)
    | r8 => bits.reverse 4 (bits.int_to_bv_n 4 8)
    | r9 => bits.reverse 4 (bits.int_to_bv_n 4 9)
    | r10 => bits.reverse 4 (bits.int_to_bv_n 4 10)
    | r11 => bits.reverse 4 (bits.int_to_bv_n 4 11)
    | r12 => bits.reverse 4 (bits.int_to_bv_n 4 12)
    | r13 => bits.reverse 4 (bits.int_to_bv_n 4 13)
    | r14 => bits.reverse 4 (bits.int_to_bv_n 4 14)
    | r15 => bits.reverse 4 (bits.int_to_bv_n 4 15)
    end.

