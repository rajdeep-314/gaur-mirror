(*
    translate.v

    Translating VM programs to PASM programs.
*)


From Stdlib Require Import List ZArith.
Import ListNotations.

From assembler Require ast result.
From vm_translator Require ast subroutines alias err.
Import alias.

Module A := assembler.ast.
Module V := vm_translator.ast.
Module S := vm_translator.subroutines.
Module R := assembler.result.
Module E := vm_translator.err.


Definition prog_res (L:Set) : Set := @R.result (A.prog L) E.t.

Definition translate_inst {L F:Set} (i:@V.inst L F) : prog_res L :=
    match i with
    | V.push seg index =>
        let indexz := Z.of_nat index in
        R.Ok (
        (* Loading the value to push into w1. *)
        match seg with
        | V.constant  => S.li w1 indexz
        | s =>
            S.load_base_addr w1 s ++
            S.li w2 indexz ++
            [ A.add L w1 w1 w2 ;
              A.ld L w1 w1 0%Z ]
        end
        (* Pushing the loaded value from w1 to the global stack. *)
        ++ S.push w1)
    | V.pop seg index =>
        let indexz := Z.of_nat index in
        match seg with
        | V.constant => R.Error E.PopToConst
        | s =>
            R.Ok (
            S.load_base_addr w1 s ++
            S.li w2 indexz ++
            [ A.add L w1 w1 w2 ] ++
            S.pop w2 ++
            [ A.st L w2 w1 0%Z ])
        end
    | _ => R.Error E.NotImplemented
    end.


