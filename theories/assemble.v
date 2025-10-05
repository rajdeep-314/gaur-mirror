From Stdlib Require Import ZArith String List.
Import ListNotations.
Open Scope string_scope.

From assembler Require reg symtable ast isa machine.


(* The opcode corresponding to an instruction. *)
Definition opcode_of_inst (inst:isa.t) : bits.bv 4 :=
    match inst with
    | isa.lli  _ _      => bits.reverse 4 (bits.int_to_bv_n 4 0)
    | isa.lui  _ _      => bits.reverse 4 (bits.int_to_bv_n 4 1)
    | isa.ld   _ _ _    => bits.reverse 4 (bits.int_to_bv_n 4 2)
    | isa.st   _ _ _    => bits.reverse 4 (bits.int_to_bv_n 4 3)
    | isa.add  _ _ _    => bits.reverse 4 (bits.int_to_bv_n 4 4)
    | isa.addi _ _ _    => bits.reverse 4 (bits.int_to_bv_n 4 5)
    | isa.sub  _ _ _    => bits.reverse 4 (bits.int_to_bv_n 4 6)
    | isa.and  _ _ _    => bits.reverse 4 (bits.int_to_bv_n 4 7)
    | isa.not  _ _      => bits.reverse 4 (bits.int_to_bv_n 4 8)
    | isa.beq  _ _ _    => bits.reverse 4 (bits.int_to_bv_n 4 9)
    | isa.blt  _ _ _    => bits.reverse 4 (bits.int_to_bv_n 4 10)
    | isa.jalr _ _ _    => bits.reverse 4 (bits.int_to_bv_n 4 11)
    | isa.apc  _ _      => bits.reverse 4 (bits.int_to_bv_n 4 12)
    | isa.j    _        => bits.reverse 4 (bits.int_to_bv_n 4 13)
    end.


(* The bit-vector corresponding to a register's index. *)
Definition reg_to_bv (r:reg.t) : bits.bv 4 :=
    match r with 
    | reg.r0    => bits.reverse 4 (bits.int_to_bv_n 4 0)
    | reg.r1    => bits.reverse 4 (bits.int_to_bv_n 4 1)
    | reg.r2    => bits.reverse 4 (bits.int_to_bv_n 4 2)
    | reg.r3    => bits.reverse 4 (bits.int_to_bv_n 4 3)
    | reg.r4    => bits.reverse 4 (bits.int_to_bv_n 4 4)
    | reg.r5    => bits.reverse 4 (bits.int_to_bv_n 4 5)
    | reg.r6    => bits.reverse 4 (bits.int_to_bv_n 4 6)
    | reg.r7    => bits.reverse 4 (bits.int_to_bv_n 4 7)
    | reg.r8    => bits.reverse 4 (bits.int_to_bv_n 4 8)
    | reg.r9    => bits.reverse 4 (bits.int_to_bv_n 4 9)
    | reg.r10   => bits.reverse 4 (bits.int_to_bv_n 4 10)
    | reg.r11   => bits.reverse 4 (bits.int_to_bv_n 4 11)
    | reg.r12   => bits.reverse 4 (bits.int_to_bv_n 4 12)
    | reg.r13   => bits.reverse 4 (bits.int_to_bv_n 4 13)
    | reg.r14   => bits.reverse 4 (bits.int_to_bv_n 4 14)
    | reg.r15   => bits.reverse 4 (bits.int_to_bv_n 4 15)
    end.


(* Expands an AST instruction to a list of ISA instructions, based on the symbol
   table tbl, with the current PC being pc. *)
Definition expand_ast_inst (l:Set) (inst:ast.t l) (tbl:symtable.t l) (eq:l -> l -> bool) (pc:nat) : isa.prog :=
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
    | ast.beq _ rs1 rs2 lbl => 
            match symtable.find_opt l eq tbl lbl with
            | Some n => [ isa.beq rs1 rs2 (bits.int_to_bv_n 4 2);
                          isa.j (bits.int_to_bv_n 12 2);
                          isa.j (bits.int_to_bv_n 12 (Z.of_nat (n - pc))) ]
            | None => [] 
            end
    | ast.blt _ rs1 rs2 lbl => 
            match symtable.find_opt l eq tbl lbl with
            | Some n => [ isa.blt rs1 rs2 (bits.int_to_bv_n 4 2);
                          isa.j (bits.int_to_bv_n 12 2);
                          isa.j (bits.int_to_bv_n 12 (Z.of_nat (n - pc))) ]
            | None => []
            end
    | ast.jalr _ rd rs imm => [isa.jalr rd rs (bits.int_to_bv_n 4 imm)]
    | ast.apc _ rd imm => [isa.apc rd (bits.int_to_bv_n 8 imm)]
    | ast.j _ lbl => 
            match symtable.find_opt l eq tbl lbl with
            | Some n => [isa.j (bits.int_to_bv_n 12 (Z.of_nat (n - pc)))]
            | None => []
            end
    | ast.label _ _ => []
    | ast.li _ rd imm =>
            let upper := Z.shiftr imm 8 in
            let lower := Z.land imm 255 in
            [ isa.lui rd (bits.int_to_bv_n 8 upper);
              isa.lli rd (bits.int_to_bv_n 8 lower) ]
    | ast.spin _ => []
    end.


(* Expands an AST program to an ISA program with the inital PC being pc. *)
Fixpoint expand_ast_prog_helper (l:Set) (p:ast.prog l) (tbl:symtable.t l) (eq:l -> l -> bool) (pc:nat) : isa.prog :=
    match p with
    | [] => []
    | inst :: rest => 
            let expanded := expand_ast_inst l inst tbl eq pc in
            let pc' := pc + List.length expanded in
             (expanded ++ expand_ast_prog_helper l rest tbl eq pc')%list
    end.

(* Translates an AST program to an ISA program. *)
Definition ast_prog_to_isa (l:Set) (eq:l -> l -> bool) (p:ast.prog l) : isa.prog :=
  let tbl := ast.symtable_of_prog l p in
  expand_ast_prog_helper l p tbl eq 0.


(* ISA instruction representation to machine representation. *)
Definition isa_inst_to_machine_inst (inst:isa.t) : machine.t :=
    match inst with 
    | isa.lli  rd imm       => (bits.concat 4 12 (opcode_of_inst inst) (bits.concat 4 8 (reg_to_bv rd) imm))
    | isa.lui  rd imm       => (bits.concat 4 12 (opcode_of_inst inst) (bits.concat 4 8 (reg_to_bv rd) imm))
    | isa.ld   rd rs imm    => (bits.concat 4 12 (opcode_of_inst inst) (bits.concat 4 8 (reg_to_bv rd) (bits.concat 4 4 (reg_to_bv rs) imm)))
    | isa.st   rs1 rs2 imm  => (bits.concat 4 12 (opcode_of_inst inst) (bits.concat 4 8 (reg_to_bv rs1) (bits.concat 4 4 (reg_to_bv rs2) imm)))
    | isa.add  rd rs1 rs2   => (bits.concat 4 12 (opcode_of_inst inst) (bits.concat 4 8 (reg_to_bv rd) (bits.concat 4 4 (reg_to_bv rs1) (reg_to_bv rs2))))
    | isa.addi rd rs imm    => (bits.concat 4 12 (opcode_of_inst inst) (bits.concat 4 8 (reg_to_bv rd) (bits.concat 4 4 (reg_to_bv rs) imm)))
    | isa.sub  rd rs1 rs2   => (bits.concat 4 12 (opcode_of_inst inst) (bits.concat 4 8 (reg_to_bv rd) (bits.concat 4 4 (reg_to_bv rs1) (reg_to_bv rs2))))
    | isa.and  rd rs1 rs2   =>  (bits.concat 4 12 (opcode_of_inst inst) (bits.concat 4 8 (reg_to_bv rd) (bits.concat 4 4 (reg_to_bv rs1) (reg_to_bv rs2))))
    | isa.not  rd rs        =>  (bits.concat 4 12 (opcode_of_inst inst)(bits.concat 4 8 (reg_to_bv rd)(bits.concat 4 4 (reg_to_bv rs) (bits.int_to_bv_n 4 0))))
    | isa.beq  rs1 rs2 imm  => (bits.concat 4 12 (opcode_of_inst inst) (bits.concat 4 8 (reg_to_bv rs1) (bits.concat 4 4 (reg_to_bv rs2) imm)))
    | isa.blt  rs1 rs2 imm  => (bits.concat 4 12 (opcode_of_inst inst) (bits.concat 4 8 (reg_to_bv rs1) (bits.concat 4 4 (reg_to_bv rs2) imm)))
    | isa.jalr rd rs imm    => (bits.concat 4 12 (opcode_of_inst inst) (bits.concat 4 8 (reg_to_bv rd) (bits.concat 4 4 (reg_to_bv rs) imm)))
    | isa.apc  rd imm       => (bits.concat 4 12 (opcode_of_inst inst)(bits.concat 4 8 (reg_to_bv rd) imm))
    | isa.j    imm          => (bits.concat 4 12 (opcode_of_inst inst) imm)
    end.


(* ISA program to it's machine representation. *)
Fixpoint isa_prog_to_machine (p:isa.prog) : machine.prog :=
    match p with 
    | [] => []
    | x :: rest => ((isa_inst_to_machine_inst x) :: isa_prog_to_machine rest)
    end. 

