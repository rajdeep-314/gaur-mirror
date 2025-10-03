From Stdlib Require Import ZArith.
From Stdlib Require Import String.
From Stdlib Require Import List.
Import ListNotations.
Open Scope string_scope.

From assembler Require reg.
From assembler Require symtable.
From assembler Require ast.
From assembler Require isa.
From assembler Require printer.
From assembler Require machine.

Module smt := symtable.
Module mc := machine.

Definition symtbl: smt.t string := smt.empty string.

Definition expand_ast (l:Set) (inst :ast.t l) (tbl:smt.t l) (eq:l -> l -> bool) (pc:nat): isa.prog :=
match inst with
| ast.lli _ rd imm => [isa.lli rd (bits.int_to_bv_n 8 imm)]
| ast.lui _ rd imm => [isa.lui rd (bits.int_to_bv_n 8 imm)]
| ast.ld _ rd rs imm => [isa.ld rd rs (bits.int_to_bv_n 4 imm)]
| ast.st _ rs1 rs2 imm => [isa.st rs1 rs2 (bits.int_to_bv_n 4 imm)]
| ast.add _ rd rs1 rs2 => [isa.add rd rs1 rs2]
| ast.addi _ rd rs imm => [isa.addi rd rs (bits.int_to_bv_n 4 imm)]
| ast.sub _ rd rs1 rs2 => [isa.sub rd rs1 rs2]
| ast.and _ rd rs1 rs2 => [isa.and rd rs1 rs2]
| ast.not _ rd rs => [isa.not rd rs]
| ast.beq _ r1 r2 lbl => 
    match smt.find_opt l eq tbl lbl with
    | Some n => [isa.beq r1 r2 (bits.int_to_bv_n 4 2);
                 isa.j (bits.int_to_bv_n 12 2);
                 isa.j (bits.int_to_bv_n 12 (Z.of_nat (n - pc)))]
    | None => [] 
    end
| ast.blt _ r1 r2 lbl => 
    match smt.find_opt l eq tbl lbl with
    | Some n => [isa.blt r1 r2 (bits.int_to_bv_n 4 2);
                 isa.j (bits.int_to_bv_n 12 2);
                 isa.j (bits.int_to_bv_n 12 (Z.of_nat (n - pc)))]
    | None => []
    end
| ast.jalr _ rd rs imm => [isa.jalr rd rs (bits.int_to_bv_n 4 imm)]
| ast.apc _ rd imm => [isa.apc rd (bits.int_to_bv_n 8 imm)]
| ast.j _ lbl => 
    match smt.find_opt l eq tbl lbl with
    | Some n => [isa.j (bits.int_to_bv_n 12 (Z.of_nat n))]
    | None => []
    end
| ast.label _ _ => []
| ast.li _ rd imm => let upper := Z.shiftr imm 8 in
                  let lower := Z.land imm 255 in
                [isa.lui rd (bits.int_to_bv_n 8 upper);
                isa.lli rd (bits.int_to_bv_n 8 lower)]
| ast.spin _ => []
end.

Fixpoint expand_prog_helper (l:Set) (p:ast.prog l) (tbl:smt.t l) (eq:l -> l -> bool) (pc:nat) : isa.prog :=
match p with
| [] => []
| inst :: rest => 
    let expanded := expand_ast l inst tbl eq pc in
    let pc' := pc + List.length expanded in
     (expanded ++ expand_prog_helper l rest tbl eq pc')%list
end.

Definition ast_to_isa (l:Set) (eq:l -> l -> bool) (p:ast.prog l) : isa.prog :=
  let tbl := ast.symtable_of_prog l p in
  expand_prog_helper l p tbl eq 0.


Definition ast_to_isa_str (p : ast.prog string) : isa.prog :=
  ast_to_isa string String.eqb p.


Definition isa_to_machine_inst (inst:isa.t):bits.bv 16 :=
  match inst with 
  | isa.lli  rd imm      => (bits.concat 4 12 (mc.opcode_of inst) (bits.concat 4 8 (mc.reg_to_bv rd) imm))
  | isa.lui  rd imm      => (bits.concat 4 12 (mc.opcode_of inst) (bits.concat 4 8 (mc.reg_to_bv rd) imm))
  | isa.ld   rd rs imm   => (bits.concat 4 12 (mc.opcode_of inst) (bits.concat 4 8 (mc.reg_to_bv rd) (bits.concat 4 4 (mc.reg_to_bv rs) imm)))
  | isa.st   rs1 rs2 imm => (bits.concat 4 12 (mc.opcode_of inst) (bits.concat 4 8 (mc.reg_to_bv rs1) (bits.concat 4 4 (mc.reg_to_bv rs2) imm)))
  | isa.add  rd rs1 rs2  => (bits.concat 4 12 (mc.opcode_of inst) (bits.concat 4 8 (mc.reg_to_bv rd) (bits.concat 4 4 (mc.reg_to_bv rs1) (mc.reg_to_bv rs2))))
  | isa.addi rd rs imm   => (bits.concat 4 12 (mc.opcode_of inst) (bits.concat 4 8 (mc.reg_to_bv rd) (bits.concat 4 4 (mc.reg_to_bv rs) imm)))
  | isa.sub  rd rs1 rs2  => (bits.concat 4 12 (mc.opcode_of inst) (bits.concat 4 8 (mc.reg_to_bv rd) (bits.concat 4 4 (mc.reg_to_bv rs1) (mc.reg_to_bv rs2))))
  | isa.and  rd rs1 rs2  =>  (bits.concat 4 12 (mc.opcode_of inst) (bits.concat 4 8 (mc.reg_to_bv rd) (bits.concat 4 4 (mc.reg_to_bv rs1) (mc.reg_to_bv rs2))))
  | isa.not  rd rs       =>  (bits.concat 4 12 (mc.opcode_of inst)(bits.concat 4 8 (mc.reg_to_bv rd)(bits.concat 4 4 (mc.reg_to_bv rs) (bits.int_to_bv_n 4 0))))
  | isa.beq  rs1 rs2 imm => (bits.concat 4 12 (mc.opcode_of inst) (bits.concat 4 8 (mc.reg_to_bv rs1) (bits.concat 4 4 (mc.reg_to_bv rs2) imm)))
  | isa.blt  rs1 rs2 imm => (bits.concat 4 12 (mc.opcode_of inst) (bits.concat 4 8 (mc.reg_to_bv rs1) (bits.concat 4 4 (mc.reg_to_bv rs2) imm)))
  | isa.jalr rd rs imm   => (bits.concat 4 12 (mc.opcode_of inst) (bits.concat 4 8 (mc.reg_to_bv rd) (bits.concat 4 4 (mc.reg_to_bv rs) imm)))
  | isa.apc  rd imm      => (bits.concat 4 12 (mc.opcode_of inst)(bits.concat 4 8 (mc.reg_to_bv rd) imm))
  | isa.j    imm          => (bits.concat 4 12 (mc.opcode_of inst) imm)
  end.

Fixpoint isa_to_machine (p:isa.prog):list (bits.bv 16):=
  match p with 
  | [] => []
  | x :: rest => ((isa_to_machine_inst x) :: isa_to_machine rest)
  end. 


Compute printer.machine_prog_to_string (isa_to_machine (ast_to_isa_str ast.sample_prog)).

