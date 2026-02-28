(*
    examples.v

    Some examples of AST VM programs.
*)


From Stdlib Require Import String List.
Import ListNotations.
Open Scope string_scope.

From vm_translator Require Import ast.


(*
function fibonacci 0
    push argument 0
    push constant 2
    lt
    if-goto if_true
    goto if_false
    label if_true
    push argument 0
    ret
    label if_false
    push argument 0
    push constant 2
    sub
    call fibonacci 1
    push argument 0
    push constant 1
    sub
    call fibonacci 1
    add
    ret

function main 0
    push constant 4
    call fibonacci 1
    label halt
    goto halt
*)

Definition fss:Set := @function string string.
Definition pss:Set := @prog string string.

Definition main_f : fss :=
    Function "main" 0 [
        push constant 4 ;
        call "fibonacci" 1 ;
        label "halt" ;
        goto "halt" ].

Definition fib_f : fss :=
    Function "fibonacci" 0 [
        push argument 0 ;
        push constant 2 ;
        lt ;
        ifgoto "if_true" ;
        goto "if_false" ;
        label "if_true" ;
        push argument 0 ;
        ret ;
        label "if_false" ;
        push argument 0 ;
        push constant 2 ;
        sub ;
        call "fibonacci" 1 ;
        push argument 0 ;
        push constant 1 ;
        sub ;
        call "fibonacci" 1 ;
        add ;
        ret ].

Definition p:pss := [ fib_f ; main_f ].

