From Stdlib Require Import String Ascii List ZArith.
From assembler Require bits reg isa.
Import reg.
Import ListNotations.
Open Scope string_scope.

Definition bit_to_string (b:bits.t) : string :=
  match b with
  | bits.b0 => "0"
  | bits.b1 => "1"
  end.

Fixpoint bv_to_binary_string {n:nat} (bvec:bits.bv n) : string :=
  match bvec with
  | bits.nil => ""
  | bits.cons _ b rest => bv_to_binary_string rest ++ bit_to_string b
  end.


Definition reg_to_string (r:reg) : string :=
  match r with
  | r0 => "r0" | r1 => "r1" | r2 => "r2" | r3 => "r3"
  | r4 => "r4" | r5 => "r5" | r6 => "r6" | r7 => "r7"
  | r8 => "r8" | r9 => "r9" | r10 => "r10" | r11 => "r11"
  | r12 => "r12" | r13 => "r13" | r14 => "r14" | r15 => "r15"
  end.

Definition instr_to_string (i:isa.t) : string :=
  match i with
  | isa.lli rd imm => "lli " ++ reg_to_string rd ++ ", " ++ bv_to_binary_string imm
  | isa.lui rd imm => "lui " ++ reg_to_string rd ++ ", " ++ bv_to_binary_string imm
  | isa.ld rd rs imm => "ld " ++ reg_to_string rd ++ ", " ++ reg_to_string rs ++ ", " ++ bv_to_binary_string imm
  | isa.st rs1 rs2 imm => "st " ++ reg_to_string rs1 ++ ", " ++ reg_to_string rs2 ++ ", " ++ bv_to_binary_string imm
  | isa.add rd rs1 rs2 => "add " ++ reg_to_string rd ++ ", " ++ reg_to_string rs1 ++ ", " ++ reg_to_string rs2
  | isa.addi rd rs imm => "addi " ++ reg_to_string rd ++ ", " ++ reg_to_string rs ++ ", " ++ bv_to_binary_string imm
  | isa.sub rd rs1 rs2 => "sub " ++ reg_to_string rd ++ ", " ++ reg_to_string rs1 ++ ", " ++ reg_to_string rs2
  | isa.and rd rs1 rs2 => "and " ++ reg_to_string rd ++ ", " ++ reg_to_string rs1 ++ ", " ++ reg_to_string rs2
  | isa.not rd rs => "not " ++ reg_to_string rd ++ ", " ++ reg_to_string rs
  | isa.beq r1 r2 imm => "beq " ++ reg_to_string r1 ++ ", " ++ reg_to_string r2 ++ ", " ++ bv_to_binary_string imm
  | isa.blt r1 r2 imm => "blt " ++ reg_to_string r1 ++ ", " ++ reg_to_string r2 ++ ", " ++ bv_to_binary_string imm
  | isa.jalr rd rs imm => "jalr " ++ reg_to_string rd ++ ", " ++ reg_to_string rs ++ ", " ++ bv_to_binary_string imm
  | isa.apc rd imm => "apc " ++ reg_to_string rd ++ ", " ++ bv_to_binary_string imm
  | isa.j imm => "j " ++ bv_to_binary_string imm
  | _ => "error"
  end.


Definition newline : string := String (Ascii.ascii_of_nat 10) "".

Fixpoint prog_to_string (p:isa.prog) : string :=
  match p with
  | [] => ""
  | [i] => instr_to_string i
  | i :: rest => instr_to_string i ++ newline ++ prog_to_string rest
  end.
