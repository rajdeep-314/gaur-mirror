(*
    isa.v

    Representation for the ISA instructions of PISA.
*)


From Stdlib Require Import List String.
Import ListNotations.
Open Scope string_scope.

From assembler Require bits reg.


Inductive t : Set :=
    | lli : reg.t -> bits.bv 8 -> t 
    | lui : reg.t -> bits.bv 8  -> t 
    | ld  : reg.t -> reg.t -> bits.bv 4 -> t 
    | st  : reg.t -> reg.t -> bits.bv 4 -> t  
    | add : reg.t -> reg.t -> reg.t -> t 
    | addi: reg.t -> reg.t -> bits.bv 4 -> t 
    | sub : reg.t -> reg.t -> reg.t -> t 
    | and : reg.t -> reg.t -> reg.t -> t 
    | not : reg.t -> reg.t -> t 
    | beq : reg.t -> reg.t -> bits.bv 4 -> t 
    | blt : reg.t -> reg.t -> bits.bv 4 -> t 
    | jalr: reg.t -> reg.t -> bits.bv 4 -> t 
    | apc : reg.t -> bits.bv 8 -> t 
    | j  : bits.bv 12 -> t .


(* A program is a list of (ISA) instructions. *)
Definition prog := list t.

(* Number of instructions in a program. *)
Definition num_insts (p:prog) : nat := List.length p.


(* String representation of an instruction. *)
Definition inst_to_string (i:t) : string :=
    match i with
    | lli rd imm =>
		 "lli " ++ reg.to_string rd ++ ", " ++ bits.bv_to_string 8 imm
    | lui rd imm =>
		 "lui " ++ reg.to_string rd ++ ", " ++ bits.bv_to_string 8 imm
    | ld rd rs imm =>
		 "ld " ++ reg.to_string rd ++ ", " ++ reg.to_string rs ++ ", " ++ bits.bv_to_string 4 imm
    | st rs1 rs2 imm =>
		 "st " ++ reg.to_string rs1 ++ ", " ++ reg.to_string rs2 ++ ", " ++ bits.bv_to_string 4 imm
    | add rd rs1 rs2 =>
		 "add " ++ reg.to_string rd ++ ", " ++ reg.to_string rs1 ++ ", " ++ reg.to_string rs2
    | addi rd rs imm =>
		 "addi " ++ reg.to_string rd ++ ", " ++ reg.to_string rs ++ ", " ++ bits.bv_to_string 4 imm
    | sub rd rs1 rs2 =>
		 "sub " ++ reg.to_string rd ++ ", " ++ reg.to_string rs1 ++ ", " ++ reg.to_string rs2
    | and rd rs1 rs2 =>
		 "and " ++ reg.to_string rd ++ ", " ++ reg.to_string rs1 ++ ", " ++ reg.to_string rs2
    | not rd rs =>
		 "not " ++ reg.to_string rd ++ ", " ++ reg.to_string rs
    | beq r1 r2 imm =>
		 "beq " ++ reg.to_string r1 ++ ", " ++ reg.to_string r2 ++ ", " ++ bits.bv_to_string 4 imm
    | blt r1 r2 imm =>
		 "blt " ++ reg.to_string r1 ++ ", " ++ reg.to_string r2 ++ ", " ++ bits.bv_to_string 4 imm
    | jalr rd rs imm =>
		 "jalr " ++ reg.to_string rd ++ ", " ++ reg.to_string rs ++ ", " ++ bits.bv_to_string 4 imm
    | apc rd imm =>
		 "apc " ++ reg.to_string rd ++ ", " ++ bits.bv_to_string 8 imm
    | j imm =>
		 "j " ++ bits.bv_to_string 12 imm
    end.


(* String representation of a program. *)
Fixpoint prog_to_string (p:prog) : string :=
    match p with
    | [] => ""
    | inst :: p' => inst_to_string inst ++ "\n" ++ prog_to_string p'
    end.

