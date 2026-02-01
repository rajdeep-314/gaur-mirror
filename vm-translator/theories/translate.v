(*
    translate.v

    Translating VM programs to PASM programs.
*)


From Stdlib Require Import List ZArith.
Import ListNotations.

From assembler Require ast result.
From vm_translator Require ast subroutines alias err.
Import result alias.

(* Module aliases. *)
Module A := assembler.ast.
Module V := vm_translator.ast.
Module S := vm_translator.subroutines.
Module E := vm_translator.err.


Definition prog_res (L:Set) : Set := @result (@A.prog L) E.t.

(*
Arguments:
    L           :   VM program's label name type
    F           :   VM program's function name type
    L'          :   PASM program's label name type
    f_name      :   Function whose body the instruction is part of
    lbl_f       :   Combine VM function name and label name to obtain PASM
                    label name
    leqb        :   Boolean equality function for VM program labels
    ls          :   List of labels declared inside the function body
    btl         :   Branch taken label for line number n
    bntl        :   Branch not taken label for line number n
    ln          :   Line number
*)
Definition translate_inst {L F L':Set}
    (f_name:F) (lbl_f:F -> L -> L')
    (leqb:L -> L -> bool) (ls:list L)
    (btl:F -> L -> nat -> L') (bntl:F -> L -> nat -> L')
    (ln:nat)
    (i:@V.inst L F) : prog_res L' :=
    match i with
    (* Stack-operation instructions. *)
    | V.push seg index =>
        let indexz := Z.of_nat index in
        Ok (
        (* Loading the value to push into w1. *)
        match seg with
        | V.constant  => S.li w1 indexz
        | s =>
            S.load_base_addr w1 s ++
            S.li w2 indexz ++
            [ A.add w1 w1 w2 ;
              A.ld w1 w1 0%Z ]
        end
        (* Pushing the loaded value from w1 to the global stack. *)
        ++ S.push w1)
    | V.pop seg index =>
        let indexz := Z.of_nat index in
        match seg with
        | V.constant => Error E.PopToConst
        | s =>
            Ok (
            S.load_base_addr w1 s ++
            S.li w2 indexz ++
            [ A.add w1 w1 w2 ] ++
            S.pop w2 ++
            [ A.st w2 w1 0%Z ])
        end
    (* Labelled instructions. *)
    | V.label l =>
        Ok [ A.label (lbl_f f_name l) ]
    | V.goto l =>
        match List.find (leqb l) ls with
        | Some _    => Ok [ A.j (lbl_f f_name l) ]
        | None      => Error E.UnboundLabel
        end
    | V.ifgoto l =>
        match List.find (leqb l) ls with
        | Some _    =>
            let target_label := lbl_f f_name l in
            let tl := btl f_name l ln in
            let ntl := bntl f_name l ln in
            Ok
            ( S.pop w1 ++
            [ A.not w1 w1 ] ++
              S.beq w1 zero target_label tl ntl)
        | None      => Error E.UnboundLabel
        end
    (* Arithmetic instructions. *)
    | V.add => Ok ( S.bin S.add )
    | V.sub => Ok ( S.bin S.sub )
    | V.neg => Ok ( S.unary S.neg )
    (* Comparison instructions. *)
    | V.eq  => Ok
        ( S.pop w1 ++ S.pop w2 ++
        [ A.sub w1 w1 w2 ;
          A.addi w1 w1 (-1)%Z ] ++
          S.push w1 )
    | V.lt  => Ok ( S.comp S.lt )
    | V.gt  => Ok ( S.comp S.lt )
    (* Boolean instructions. *)
    | V.and => Ok ( S.bin S.and )
    | V.or  => Ok ( S.bin S.or )
    | V.not => Ok ( S.unary S.not )
    | _ => Error E.NotImplemented
    end.


