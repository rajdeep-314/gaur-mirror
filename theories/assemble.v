From Stdlib Require Import ZArith.
From Stdlib Require Import String.
From Stdlib Require Import List.
Import ListNotations.
Open Scope string_scope.

From assembler Require symtable.
From assembler Require ast.
From assembler Require isa.

Module smt := symtable.

Definition symtbl: smt.t string := smt.empty string.

(* Definition expand_ast (l:Set) (inst :ast.t l) (tbl:smt.t l) (eq:l -> l -> bool) (pc:nat): isa.prog l :=
    match inst with
    | ast.beq _ r1 r2 lbl => let n := tbl.find_opt l eq tbl lbl in 
            [isa.beq r1 r2 2;isa.j 2;isa.j (n-pc%Z)] 
    | ast.blt _ r1 r2 lbl  => let n := tbl.find_opt l eq tbl lbl in 
            [isa.blt r1 r2 2;isa.j 2;isa.j (n-pc%Z)] 
    | ast.li _ r i => [isa.lui ]
    | ast.l =>
    | _ => *)