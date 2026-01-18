(*
    isa.v

    Representation for the ISA instructions of PISA.
*)


From Stdlib Require Import ZArith List String.
Import ListNotations.
Open Scope string_scope.

From assembler Require bits reg.


Inductive t:Set :=
    | lli   : reg.t -> bits.bv 8 -> t 
    | lui   : reg.t -> bits.bv 8  -> t 
    | ld    : reg.t -> reg.t -> bits.bv 4 -> t 
    | st    : reg.t -> reg.t -> bits.bv 4 -> t  
    | add   : reg.t -> reg.t -> reg.t -> t 
    | addi  : reg.t -> reg.t -> bits.bv 4 -> t 
    | sub   : reg.t -> reg.t -> reg.t -> t 
    | and   : reg.t -> reg.t -> reg.t -> t 
    | not   : reg.t -> reg.t -> t 
    | beq   : reg.t -> reg.t -> bits.bv 4 -> t 
    | blt   : reg.t -> reg.t -> bits.bv 4 -> t 
    | jalr  : reg.t -> reg.t -> bits.bv 4 -> t 
    | apc   : reg.t -> bits.bv 8 -> t 
    | j     : bits.bv 12 -> t .


(* A program is a list of (ISA) instructions. *)
Definition prog := list t.

(* Number of instructions in a program. *)
Definition num_insts (p:prog) : nat := List.length p.


(* For extraction. *)

(* The ISA type with integers instead of imms. *)
Inductive t_:Set :=
    | lli_  : reg.t -> Z -> t_
    | lui_  : reg.t -> Z  -> t_
    | ld_   : reg.t -> reg.t -> Z -> t_
    | st_   : reg.t -> reg.t -> Z -> t_
    | add_  : reg.t -> reg.t -> reg.t -> t_
    | addi_ : reg.t -> reg.t -> Z -> t_
    | sub_  : reg.t -> reg.t -> reg.t -> t_
    | and_  : reg.t -> reg.t -> reg.t -> t_
    | not_  : reg.t -> reg.t -> t_
    | beq_  : reg.t -> reg.t -> Z -> t_
    | blt_  : reg.t -> reg.t -> Z -> t_
    | jalr_ : reg.t -> reg.t -> Z -> t_
    | apc_  : reg.t -> Z -> t_
    | j_    : Z -> t_.

Definition prog_erased := list t_.

Definition erase (inst:t) : t_ :=
    let f := bits.bv_to_int in
    match inst with
    | lli rd imm        => lli_ rd (f 8 imm)
    | lui rd imm        => lui_ rd (f 8 imm)
    | ld rd rs imm      => ld_ rd rs (f 4 imm)
    | st rd rs imm      => st_ rd rs (f 4 imm)
    | add rd rs1 rs2    => add_ rd rs1 rs2
    | addi rd rs1 imm   => addi_ rd rs1 (f 4 imm)
    | sub rd rs1 rs2    => sub_ rd rs1 rs2
    | and rd rs1 rs2    => and_ rd rs1 rs2
    | not rd rs         => not_ rd rs
    | beq rs1 rs2 imm   => beq_ rs1 rs2 (f 4 imm)
    | blt rs1 rs2 imm   => blt_ rs1 rs2 (f 4 imm)
    | jalr rd rs imm    => jalr_ rd rs (f 4 imm)
    | apc rd imm        => apc_ rd (f 8 imm)
    | j imm             => j_ (f 12 imm)
    end.

