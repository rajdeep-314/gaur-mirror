(*
    assemble.v

    Functions for translating programs from assembly to
    - ISA programs
    - Machine programs
*)


From Stdlib Require Import ZArith String List.
Import ListNotations.
Open Scope string_scope.

From assembler Require result reg symtable ast isa machine.
Import result.


(* Width of an instruction represents how many ISA instructions it
expands to.
This is used to resolve labels. *)
Definition width (l:Set) (inst:ast.t l) : nat :=
    match inst with
    | ast.label _ _ => 0
    | _ => 1
    end.

(* Builds a symbol table out of the labels in a program, with count as the
   base line number. *)
Fixpoint build_symtable (l:Set) (p:ast.prog l) (count:nat) : symtable.t l :=
    match p with
    | [] => symtable.empty l
    | i :: p' =>
            let count' := count + width l i in
            match i with
            | ast.label _ name => symtable.add l (build_symtable l p' count') name count
            | _ => build_symtable l p' count'
            end
    end.

(* Builds a symbol table of a program (with base line number as 0). *)
Definition symtable_of_prog (l:Set) (p:ast.prog l) : symtable.t l :=
    build_symtable l p 0.


(* Translate an assembly instruction to it's corresponding ISA instruction. *)
Definition ast_to_isa (l:Set) (eqb:l -> l -> bool) (inst:ast.t l) (tbl:symtable.t l) (pc:nat) : result (option isa.t) string :=
    match inst with
    | ast.lli _ rd imm => Ok (Some (isa.lli rd (bits.int_to_bv_n 8 imm)))
    | ast.lui _ rd imm => Ok (Some (isa.lui rd (bits.int_to_bv_n 8 imm)))
    | ast.ld _ rd rs imm => Ok (Some (isa.ld rd rs (bits.int_to_bv_n 4 imm)))
    | ast.st _ rs1 rs2 imm => Ok (Some (isa.st rs1 rs2 (bits.int_to_bv_n 4 imm)))
    | ast.add _ rd rs1 rs2 => Ok (Some (isa.add rd rs1 rs2))
    | ast.addi _ rd rs imm => Ok (Some (isa.addi rd rs (bits.int_to_bv_n 4 imm)))
    | ast.sub _ rd rs1 rs2 => Ok (Some (isa.sub rd rs1 rs2))
    | ast.and _ rd rs1 rs2 => Ok (Some (isa.and rd rs1 rs2))
    | ast.not _ rd rs => Ok (Some (isa.not rd rs))
    | ast.beq _ rs1 rs2 lbl =>
            match symtable.find_opt l eqb tbl lbl with
            | Some n =>
                    let n_z := Z.of_nat n in
                    let pc_z := Z.of_nat pc in
                    let delta := (n_z - pc_z)%Z in
                    if andb (Z.leb (-8) delta) (Z.leb delta 7) then
                        Ok (Some (isa.beq rs1 rs2 (bits.int_to_bv_n 4 delta)))
                    else Error "Branch offset outside the [-8, 7] interval (beq)"
            | None => Error "Label not found (beq)"
            end
    | ast.blt _ rs1 rs2 lbl =>
            match symtable.find_opt l eqb tbl lbl with
            | Some n =>
                    let n_z := Z.of_nat n in
                    let pc_z := Z.of_nat pc in
                    let delta := (n_z - pc_z)%Z in
                    if andb (Z.leb (-8) delta) (Z.leb delta 7) then
                        Ok (Some (isa.blt rs1 rs2 (bits.int_to_bv_n 4 delta)))
                    else Error "Branch offset outside the [-8, 7] interval (blt)"
            | None => Error "Label not found (blt)"
            end
    | ast.jalr _ rd rs imm => Ok (Some (isa.jalr rd rs (bits.int_to_bv_n 4 imm)))
    | ast.apc _ rd imm => Ok (Some (isa.apc rd (bits.int_to_bv_n 8 imm)))
    | ast.j _ lbl =>
            match symtable.find_opt l eqb tbl lbl with
            | Some n =>
                    let n_z := Z.of_nat n in
                    let pc_z := Z.of_nat pc in
                    let delta := (n_z - pc_z)%Z in
                    if andb (Z.leb (-2048) delta) (Z.leb delta 2047) then
                        Ok (Some (isa.j (bits.int_to_bv_n 12 delta)))
                    else Error "Jump offset outside the [-2048, 2047] interval (j)"
            | None => Error "Label not found (j)"
            end
    | ast.label _ _ => Ok None
    | ast.spin _ => Ok (Some (isa.j (bits.int_to_bv_n 12 0)))
    end.


Fixpoint ast_prog_to_isa_prog_helper (l:Set) (eqb:l -> l -> bool) (p:ast.prog l) (tbl:symtable.t l) (pc:nat) : result isa.prog string :=
    match p with
    | [] =>  Ok []
    | ast.label _ _ :: p' => ast_prog_to_isa_prog_helper l eqb p' tbl pc
    | i :: p' =>
            match ast_to_isa l eqb i tbl pc with
            | Ok None => ast_prog_to_isa_prog_helper l eqb p' tbl pc
            | Ok (Some inst) =>
                    match ast_prog_to_isa_prog_helper l eqb p' tbl (S pc) with
                    | Ok prog => Ok (inst :: prog)
                    | error => error
                    end
            | Error e => Error e
            end
    end.


(* Translate an assembly program to it's corresponding ISA program. *)
Definition ast_prog_to_isa_prog (l:Set) (eqb:l -> l -> bool) (p:ast.prog l) : result isa.prog string :=
    let tbl := symtable_of_prog l p in
    ast_prog_to_isa_prog_helper l eqb p tbl 0.



(* The opcode corresponding to an instruction. *)
Definition opcode_of_inst (inst:isa.t) : bits.bv 4 :=
    match inst with
    | isa.lli  _ _      => bits.reverse (bits.int_to_bv_n 4 0)
    | isa.lui  _ _      => bits.reverse (bits.int_to_bv_n 4 1)
    | isa.ld   _ _ _    => bits.reverse (bits.int_to_bv_n 4 2)
    | isa.st   _ _ _    => bits.reverse (bits.int_to_bv_n 4 3)
    | isa.add  _ _ _    => bits.reverse (bits.int_to_bv_n 4 4)
    | isa.addi _ _ _    => bits.reverse (bits.int_to_bv_n 4 5)
    | isa.sub  _ _ _    => bits.reverse (bits.int_to_bv_n 4 6)
    | isa.and  _ _ _    => bits.reverse (bits.int_to_bv_n 4 7)
    | isa.not  _ _      => bits.reverse (bits.int_to_bv_n 4 8)
    | isa.beq  _ _ _    => bits.reverse (bits.int_to_bv_n 4 9)
    | isa.blt  _ _ _    => bits.reverse (bits.int_to_bv_n 4 10)
    | isa.jalr _ _ _    => bits.reverse (bits.int_to_bv_n 4 11)
    | isa.apc  _ _      => bits.reverse (bits.int_to_bv_n 4 12)
    | isa.j    _        => bits.reverse (bits.int_to_bv_n 4 13)
    end.


(* The bit-vector corresponding to a register's index. *)
Definition reg_to_bv (r:reg.t) : bits.bv 4 :=
    match r with 
    | reg.r0    => bits.reverse (bits.int_to_bv_n 4 0)
    | reg.r1    => bits.reverse (bits.int_to_bv_n 4 1)
    | reg.r2    => bits.reverse (bits.int_to_bv_n 4 2)
    | reg.r3    => bits.reverse (bits.int_to_bv_n 4 3)
    | reg.r4    => bits.reverse (bits.int_to_bv_n 4 4)
    | reg.r5    => bits.reverse (bits.int_to_bv_n 4 5)
    | reg.r6    => bits.reverse (bits.int_to_bv_n 4 6)
    | reg.r7    => bits.reverse (bits.int_to_bv_n 4 7)
    | reg.r8    => bits.reverse (bits.int_to_bv_n 4 8)
    | reg.r9    => bits.reverse (bits.int_to_bv_n 4 9)
    | reg.r10   => bits.reverse (bits.int_to_bv_n 4 10)
    | reg.r11   => bits.reverse (bits.int_to_bv_n 4 11)
    | reg.r12   => bits.reverse (bits.int_to_bv_n 4 12)
    | reg.r13   => bits.reverse (bits.int_to_bv_n 4 13)
    | reg.r14   => bits.reverse (bits.int_to_bv_n 4 14)
    | reg.r15   => bits.reverse (bits.int_to_bv_n 4 15)
    end.


(* ISA instruction representation to machine representation. *)
Definition isa_to_machine (inst:isa.t) : machine.t :=
    match inst with 
    | isa.lli  rd imm       => bits.concat 4 12 (opcode_of_inst inst) (bits.concat 4 8 (reg_to_bv rd) (bits.reverse imm))
    | isa.lui  rd imm       => bits.concat 4 12 (opcode_of_inst inst) (bits.concat 4 8 (reg_to_bv rd) (bits.reverse imm))
    | isa.ld   rd rs imm    => bits.concat 4 12 (opcode_of_inst inst) (bits.concat 4 8 (reg_to_bv rd) (bits.concat 4 4 (reg_to_bv rs) (bits.reverse imm)))
    | isa.st   rs1 rs2 imm  => bits.concat 4 12 (opcode_of_inst inst) (bits.concat 4 8 (reg_to_bv rs1) (bits.concat 4 4 (reg_to_bv rs2) (bits.reverse imm)))
    | isa.add  rd rs1 rs2   => bits.concat 4 12 (opcode_of_inst inst) (bits.concat 4 8 (reg_to_bv rd) (bits.concat 4 4 (reg_to_bv rs1) (reg_to_bv rs2)))
    | isa.addi rd rs imm    => bits.concat 4 12 (opcode_of_inst inst) (bits.concat 4 8 (reg_to_bv rd) (bits.concat 4 4 (reg_to_bv rs) (bits.reverse imm)))
    | isa.sub  rd rs1 rs2   => bits.concat 4 12 (opcode_of_inst inst) (bits.concat 4 8 (reg_to_bv rd) (bits.concat 4 4 (reg_to_bv rs1) (reg_to_bv rs2)))
    | isa.and  rd rs1 rs2   => bits.concat 4 12 (opcode_of_inst inst) (bits.concat 4 8 (reg_to_bv rd) (bits.concat 4 4 (reg_to_bv rs1) (reg_to_bv rs2)))
    | isa.not  rd rs        => bits.concat 4 12 (opcode_of_inst inst)(bits.concat 4 8 (reg_to_bv rd)(bits.concat 4 4 (reg_to_bv rs) (bits.int_to_bv_n 4 0)))
    | isa.beq  rs1 rs2 imm  => bits.concat 4 12 (opcode_of_inst inst) (bits.concat 4 8 (reg_to_bv rs1) (bits.concat 4 4 (reg_to_bv rs2) (bits.reverse imm)))
    | isa.blt  rs1 rs2 imm  => bits.concat 4 12 (opcode_of_inst inst) (bits.concat 4 8 (reg_to_bv rs1) (bits.concat 4 4 (reg_to_bv rs2) (bits.reverse imm)))
    | isa.jalr rd rs imm    => bits.concat 4 12 (opcode_of_inst inst) (bits.concat 4 8 (reg_to_bv rd) (bits.concat 4 4 (reg_to_bv rs) (bits.reverse imm)))
    | isa.apc  rd imm       => bits.concat 4 12 (opcode_of_inst inst)(bits.concat 4 8 (reg_to_bv rd) (bits.reverse imm))
    | isa.j    imm          => bits.concat 4 12 (opcode_of_inst inst) (bits.reverse imm)
    end.


(* ISA program to it's machine representation. *)
Definition isa_prog_to_machine_prog : isa.prog -> machine.prog :=
    List.map isa_to_machine.


(* Translate an assembly program to a machine program. *)
Definition assemble_prog (l:Set) (eqb:l -> l -> bool) (p:ast.prog l) : result machine.prog string :=
    match ast_prog_to_isa_prog l eqb p with
    | Ok isa_p => Ok (isa_prog_to_machine_prog isa_p)
    | Error e => Error e
    end.

(* Assemble multiple assembly programs to a machine program.
   The entry point is assumed to be the starting of the first program in the list. *)
Definition assemble_progs (l:Set) (eqb:l -> l -> bool) (ps:list (ast.prog l)) : result machine.prog string :=
    assemble_prog l eqb (List.concat ps).

