(*
    errors.v
*)


From Stdlib Require Import String ZArith.

From assembler Require Import utils.

Open Scope string.


Inductive err {A:Type} : Type :=
    | UndefinedLabel  : string -> A -> err
    | DuplicateLabels : string -> A -> list (nat * A) -> err
    | OOBBeq          : string -> A -> Z -> err
    | OOBBlt          : string -> A -> Z -> err
    | OOBJ            : string -> A -> Z -> err.

Arguments err A : clear implicits.


Definition to_string {A:Type} (p:A -> string) (e:err A) : string :=
    let nts := nat_to_string in
    let zts := Z_to_string in
    match e with
    | UndefinedLabel s a =>
        p a ++
        "\nUndefined label " ++ s
    | DuplicateLabels s a xas =>
        p a ++
        "\nDuplicate label definitions for " ++ s ++
        "\nInstances:\n" ++
        String.concat "\n" (List.map
            (fun '(n, a) => "Target PC = " ++ nts n ++ ":\n" ++ p a) xas)
    | OOBBeq l a n =>
        let '(lb, ub) := po2_signed_range 6 in
        p a ++
        "\nBranch (beq) target " ++ l ++ "'s offset is outside the valid range" ++
        "\n" ++ zts n ++ " is outside [" ++ zts lb ++ ", " ++ zts ub ++ "]"
    | OOBBlt l a n =>
        let '(lb, ub) := po2_signed_range 6 in
        p a ++
        "\nBranch (blt) target " ++ l ++ "'s offset is outside the valid range" ++
        "\n" ++ zts n ++ " is outside [" ++ zts lb ++ ", " ++ zts ub ++ "]"
    | OOBJ l a n =>
        let '(lb, ub) := po2_signed_range 16 in
        p a ++
        "\nJump (j) target " ++ l ++ "'s offset is outside the valid range" ++
        "\n" ++ zts n ++ " is outside [" ++ zts lb ++ ", " ++ zts ub ++ "]"
    end.

