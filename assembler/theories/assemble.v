(*
    assemble.v
*)


(**
    {1 Assemble Module}

    This module contains functions to translate assembly programs
    to ISA programs and machine programs.
*)


From Stdlib Require Import ZArith String List.
Import ListNotations.
Open Scope string_scope.

From assembler_new Require utils result reg symtable ast isa machine.
Import result.


(**
    width : Given an assembly instruction, returns the number of ISA instructions.
    Used to reslove labels.
*)
Definition width (l:Set) (inst:@ast.t l) : nat :=
    match inst with
    | ast.label _   => 0
    | _             => 1
    end.

(**
    Builds a symbol table out of the labels in a program, with count as the
    base line number. 
*)
Fixpoint build_symtable (l:Set) (p:@ast.prog l) (count:nat) : symtable.t l :=
    match p with
    | []        => symtable.empty l
    | i :: p'   =>
            let count' := count + width l i in
            match i with
            | ast.label name => symtable.add l (build_symtable l p' count') name count
            | _ => build_symtable l p' count'
            end
    end.

(**
    Builds a symbol table of a program (with base line number as 0). 
*)
Definition symtable_of_prog (l:Set) (p:@ast.prog l) : symtable.t l :=
    build_symtable l p 0.


(** Translate an assembly instruction to it's corresponding Gaur ISA instruction. *)
Definition ast_to_isa (l:Set) (eqb:l -> l -> bool) (inst:@ast.t l) (tbl:symtable.t l) (pc:nat) : result (option isa.t) string :=
    match inst with
    | ast.li rd imm        => Ok (Some (isa.li rd (bits.int_to_bv_n 11 imm)))
    | ast.ld  rd rs imm    => Ok (Some (isa.ld rd rs (bits.int_to_bv_n 6 imm)))
    | ast.st  rs1 rs2 imm  => Ok (Some (isa.st rs1 rs2 (bits.int_to_bv_n 6 imm)))
    | ast.add  rd rs1 rs2  => Ok (Some (isa.add rd rs1 rs2))
    | ast.addi  rd rs imm  => Ok (Some (isa.addi rd rs (bits.int_to_bv_n 6 imm)))
    | ast.sub rd rs1 rs2  => Ok (Some (isa.sub rd rs1 rs2))
    | ast.and rd rs1 rs2  => Ok (Some (isa.and rd rs1 rs2))
    | ast.not rd rs       => Ok (Some (isa.not rd rs))
    | ast.sl rd rs imm    => Ok (Some (isa.sl rd rs (bits.int_to_bv_n 5 imm)))
    | ast.sr rd rs imm    => Ok (Some (isa.sr rd rs (bits.int_to_bv_n 5 imm)))
    | ast.beq rs1 rs2 lbl =>
            match symtable.find_opt l eqb tbl lbl with
            | Some n    =>
                    let n_z := Z.of_nat n in
                    let pc_z := Z.of_nat pc in
                    let delta := (n_z - pc_z)%Z in
                    if andb (Z.leb (-32) delta) (Z.leb delta 31) then
                        Ok (Some (isa.beq rs1 rs2 (bits.int_to_bv_n 6 delta)))
                    else Error "Branch offset outside the [-32, 31] interval (beq)"
            | None      => Error "Label not found (beq)"
            end
    | ast.blt rs1 rs2 lbl =>
            match symtable.find_opt l eqb tbl lbl with
            | Some n    =>
                    let n_z := Z.of_nat n in
                    let pc_z := Z.of_nat pc in
                    let delta := (n_z - pc_z)%Z in
                    if andb (Z.leb (-32) delta) (Z.leb delta 31) then
                        Ok (Some (isa.blt rs1 rs2 (bits.int_to_bv_n 6 delta)))
                    else Error "Branch offset outside the [-32, 31] interval (blt)"
            | None      => Error "Label not found (blt)"
            end
    | ast.jalr rd rs imm  => Ok (Some (isa.jalr rd rs (bits.int_to_bv_n 6 imm)))
    | ast.apc rd imm      => Ok (Some (isa.apc rd (bits.int_to_bv_n 11 imm)))
    | ast.j lbl           =>
            match symtable.find_opt l eqb tbl lbl with
            | Some n    =>
                    let n_z := Z.of_nat n in
                    let pc_z := Z.of_nat pc in
                    let delta := (n_z - pc_z)%Z in
                    if andb (Z.leb (-32768) delta) (Z.leb delta 32767) then
                        Ok (Some (isa.j (bits.int_to_bv_n 16 delta)))
                    else Error "Jump offset outside the [-32768, 32768] interval (j)"
            | None      => Error "Label not found (j)"
            end
    | ast.label _         => Ok None
    | ast.spin            => Ok (Some (isa.j (bits.int_to_bv_n 16 0)))
    end.

(** Helper function for translating ast to isa *)
Fixpoint ast_prog_to_isa_prog_helper (l:Set) (eqb:l -> l -> bool) (p:@ast.prog l) (tbl:symtable.t l) (pc:nat) : result isa.prog string :=
    match p with
    | []                    =>  Ok []
    | ast.label _ :: p'     => ast_prog_to_isa_prog_helper l eqb p' tbl pc
    | i :: p' =>
            match ast_to_isa l eqb i tbl pc with
            | Ok None           => ast_prog_to_isa_prog_helper l eqb p' tbl pc
            | Ok (Some inst)    =>
                    match ast_prog_to_isa_prog_helper l eqb p' tbl (S pc) with
                    | Ok prog   => Ok (inst :: prog)
                    | error     => error
                    end
            | Error e           => Error e
            end
    end.


(** Translate an assembly program to it's corresponding ISA program. *)
Definition ast_prog_to_isa_prog (l:Set) (eqb:l -> l -> bool) (sf:l -> string) (p:@ast.prog l) : result isa.prog string :=
    let tbl := symtable_of_prog l p in
    match symtable.dup l eqb tbl with
    | None              => ast_prog_to_isa_prog_helper l eqb p tbl 0
    | Some (lbl, n, n') =>
            let n_str   := utils.nat_to_string n in
            let n'_str  := utils.nat_to_string n' in
            let nl      := utils.new_line in
            let tab     := utils.tab in
            result.Error ( "Error:" ++ nl ++ tab ++ "Label " ++ sf lbl ++ " is defined at multiple places." ++
                    nl ++ tab ++ "Corresponds to line numbers " ++ n_str ++ " and " ++ n'_str ++ ", for example." ++ nl )
    end.


(** Translate a list of assembly programs to their corresponding ISA program. *)
Definition ast_progs_to_isa_prog (l:Set) (eqb:l -> l -> bool) (sf:l -> string) (ps:list (@ast.prog l)) : result isa.prog string :=
    ast_prog_to_isa_prog l eqb sf (List.concat ps).


(** The opcode corresponding to an instruction. *)
Definition opcode_of_inst (inst:isa.t) : bits.bv 16 :=
    match inst with
    | isa.li  _ _       => bits.reverse (bits.int_to_bv_n 16 1)
    | isa.ld   _ _ _    => bits.reverse (bits.int_to_bv_n 16 2)
    | isa.st   _ _ _    => bits.reverse (bits.int_to_bv_n 16 4)
    | isa.add  _ _ _    => bits.reverse (bits.int_to_bv_n 16 8)
    | isa.addi _ _ _    => bits.reverse (bits.int_to_bv_n 16 16)
    | isa.sub  _ _ _    => bits.reverse (bits.int_to_bv_n 16 32)
    | isa.and  _ _ _    => bits.reverse (bits.int_to_bv_n 16 64)
    | isa.not  _ _      => bits.reverse (bits.int_to_bv_n 16 128)
    | isa.sl  _ _ _     => bits.reverse (bits.int_to_bv_n 16 256)
    | isa.sr _ _ _      => bits.reverse (bits.int_to_bv_n 16 512)
    | isa.beq  _ _ _    => bits.reverse (bits.int_to_bv_n 16 1024)
    | isa.blt  _ _ _    => bits.reverse (bits.int_to_bv_n 16 2048)
    | isa.jalr _ _ _    => bits.reverse (bits.int_to_bv_n 16 4096)
    | isa.apc  _ _      => bits.reverse (bits.int_to_bv_n 16 8192)
    | isa.j    _        => bits.reverse (bits.int_to_bv_n 16 16384)
    end.


(** The bit-vector corresponding to a register's index. *)
Definition reg_to_bv (r:reg.t) : bits.bv 5 :=
    match r with 
    | reg.r0    => bits.reverse (bits.int_to_bv_n 5 0)
    | reg.r1    => bits.reverse (bits.int_to_bv_n 5 1)
    | reg.r2    => bits.reverse (bits.int_to_bv_n 5 2)
    | reg.r3    => bits.reverse (bits.int_to_bv_n 5 3)
    | reg.r4    => bits.reverse (bits.int_to_bv_n 5 4)
    | reg.r5    => bits.reverse (bits.int_to_bv_n 5 5)
    | reg.r6    => bits.reverse (bits.int_to_bv_n 5 6)
    | reg.r7    => bits.reverse (bits.int_to_bv_n 5 7)
    | reg.r8    => bits.reverse (bits.int_to_bv_n 5 8)
    | reg.r9    => bits.reverse (bits.int_to_bv_n 5 9)
    | reg.r10   => bits.reverse (bits.int_to_bv_n 5 10)
    | reg.r11   => bits.reverse (bits.int_to_bv_n 5 11)
    | reg.r12   => bits.reverse (bits.int_to_bv_n 5 12)
    | reg.r13   => bits.reverse (bits.int_to_bv_n 5 13)
    | reg.r14   => bits.reverse (bits.int_to_bv_n 5 14)
    | reg.r15   => bits.reverse (bits.int_to_bv_n 5 15)
    | reg.r16   => bits.reverse (bits.int_to_bv_n 5 16)
    | reg.r17   => bits.reverse (bits.int_to_bv_n 5 17)
    | reg.r18   => bits.reverse (bits.int_to_bv_n 5 18)
    | reg.r19   => bits.reverse (bits.int_to_bv_n 5 19)
    | reg.r20   => bits.reverse (bits.int_to_bv_n 5 20)
    | reg.r21   => bits.reverse (bits.int_to_bv_n 5 21)
    | reg.r22   => bits.reverse (bits.int_to_bv_n 5 22)
    | reg.r23   => bits.reverse (bits.int_to_bv_n 5 23)
    | reg.r24   => bits.reverse (bits.int_to_bv_n 5 24)
    | reg.r25   => bits.reverse (bits.int_to_bv_n 5 25)
    | reg.r26   => bits.reverse (bits.int_to_bv_n 5 26)
    | reg.r27   => bits.reverse (bits.int_to_bv_n 5 27)
    | reg.r28   => bits.reverse (bits.int_to_bv_n 5 28)
    | reg.r29   => bits.reverse (bits.int_to_bv_n 5 29)
    | reg.r30   => bits.reverse (bits.int_to_bv_n 5 30)
    | reg.r31   => bits.reverse (bits.int_to_bv_n 5 31)
    end.


(** ISA instruction representation to machine representation. *)
Definition isa_to_machine (inst:isa.t) : machine.t :=
    match inst with
    | isa.li  rd imm        => bits.concat 16 16 (opcode_of_inst inst) (bits.concat 5 11 (reg_to_bv rd) (bits.reverse imm))
    | isa.ld   rd rs imm    => bits.concat 16 16 (opcode_of_inst inst) (bits.concat 5 11 (reg_to_bv rd) (bits.concat 5 6 (reg_to_bv rs) (bits.reverse imm)))
    | isa.st   rs1 rs2 imm  => bits.concat 16 16 (opcode_of_inst inst) (bits.concat 5 11 (reg_to_bv rs1) (bits.concat 5 6 (reg_to_bv rs2) (bits.reverse imm)))
    | isa.add  rd rs1 rs2   => bits.concat 16 16 (opcode_of_inst inst) (bits.concat 5 11 (reg_to_bv rd) (bits.concat 5 6 (reg_to_bv rs1) (bits.concat 5 1 (reg_to_bv rs2) (bits.int_to_bv_n 1 0))))
    | isa.addi rd rs imm    => bits.concat 16 16 (opcode_of_inst inst) (bits.concat 5 11 (reg_to_bv rd) (bits.concat 5 6 (reg_to_bv rs) (bits.reverse imm)))
    | isa.sub  rd rs1 rs2   => bits.concat 16 16 (opcode_of_inst inst) (bits.concat 5 11 (reg_to_bv rd) (bits.concat 5 6 (reg_to_bv rs1) (bits.concat 5 1 (reg_to_bv rs2) (bits.int_to_bv_n 1 0))))
    | isa.and  rd rs1 rs2   => bits.concat 16 16 (opcode_of_inst inst) (bits.concat 5 11 (reg_to_bv rd) (bits.concat 5 6 (reg_to_bv rs1) (bits.concat 5 1 (reg_to_bv rs2) (bits.int_to_bv_n 1 0))))
    | isa.not  rd rs        => bits.concat 16 16 (opcode_of_inst inst) (bits.concat 5 11 (reg_to_bv rd) (bits.concat 5 6 (reg_to_bv rs) (bits.int_to_bv_n 6 0)))
    | isa.sl   rd rs imm    => bits.concat 16 16 (opcode_of_inst inst) (bits.concat 5 11 (reg_to_bv rd) (bits.concat 5 6 (reg_to_bv rs) (bits.concat 1 5 (bits.int_to_bv_n 1 0) (bits.reverse imm))))
    | isa.sr   rd rs imm    => bits.concat 16 16 (opcode_of_inst inst) (bits.concat 5 11 (reg_to_bv rd) (bits.concat 5 6 (reg_to_bv rs) (bits.concat 1 5 (bits.int_to_bv_n 1 0) (bits.reverse imm))))
    (*| isa.sr   rd rs imm    => bits.concat 16 16 (opcode_of_inst inst) (bits.concat 5 11 (reg_to_bv rd) (bits.concat 5 6 (reg_to_bv rs) (bits.b0 :: bits.reverse imm)))*)
    | isa.beq  rs1 rs2 imm  => bits.concat 16 16 (opcode_of_inst inst) (bits.concat 5 11 (reg_to_bv rs1) (bits.concat 5 6 (reg_to_bv rs2) (bits.reverse imm)))
    | isa.blt  rs1 rs2 imm  => bits.concat 16 16 (opcode_of_inst inst) (bits.concat 5 11 (reg_to_bv rs1) (bits.concat 5 6 (reg_to_bv rs2) (bits.reverse imm)))
    | isa.jalr rd rs imm    => bits.concat 16 16 (opcode_of_inst inst) (bits.concat 5 11 (reg_to_bv rd) (bits.concat 5 6 (reg_to_bv rs) (bits.reverse imm)))
    | isa.apc  rd imm       => bits.concat 16 16 (opcode_of_inst inst) (bits.concat 5 11 (reg_to_bv rd) (bits.reverse imm))
    | isa.j    imm          => bits.concat 16 16 (opcode_of_inst inst) (bits.reverse imm)
    end.


(** ISA program to it's machine representation. *)
Definition isa_prog_to_machine_prog : isa.prog -> machine.prog :=
    List.map isa_to_machine.


(** Translate an assembly program to a machine program. *)
Definition assemble_prog (l:Set) (eqb:l -> l -> bool) (sf:l -> string) (p:@ast.prog l) : result machine.prog string :=
    match ast_prog_to_isa_prog l eqb sf p with
    | Ok isa_p  => Ok (isa_prog_to_machine_prog isa_p)
    | Error e   => Error e
    end.

(** Assemble multiple assembly programs to a machine program.
   The entry point is assumed to be the starting of the first program in the list. *)
Definition assemble_progs (l:Set) (eqb:l -> l -> bool) (sf:l -> string) (ps:list (@ast.prog l)) : result machine.prog string :=
    assemble_prog l eqb sf (List.concat ps).


(* For extraction. *)

Definition ast_prog_to_isa_prog_extr (l:Set) (eqb:l -> l -> bool) (sf:l -> string) (p:@ast.prog l) : result isa.prog_erased string :=
    result.apply (List.map isa.erase) (ast_prog_to_isa_prog l eqb sf p).

Definition assemble_prog_extr (l:Set) (eqb:l -> l -> bool) (sf:l -> string) (p:@ast.prog l) : result machine.prog_erased string :=
    result.apply (List.map (bits.erase 32)) (assemble_prog l eqb sf p).

