let () = print_endline "CPU simulator for PISA."


(* Testing. *)

module Proc = Cpusim.Proc
module Ast = Cpusim.Ast
module Rf = Cpusim.Regfile

open Ast.Reg.Helper
open Ast.Helper

(* Odd or even. *)
let _prog1 : Ast.prog =
    [
        lui r1 0;
        lli r1 11;

        lui r2 0;
        lli r2 1;
        and_ r3 r1 r2;
        beq r3 r0 4;
        lui r4 0xff;
        lli r4 0xff;
        j 3;
        lui r4 0;
        lli r4 1;
    ]

(* Max of 3. *)
let _prog2 : Ast.prog =
    [
        lli r1 4;
        lli r2 68;
        lli r3 68;

        blt r1 r2 2;
        j 2;
        j 3;
        add r4 r0 r1;
        j 2;
        add r4 r0 r2;
        blt r4 r3 2;
        j 2;
        j 3;
        add r10 r0 r4;
        j 2;
        add r10 r0 r3;
    ]


(* Sum of the first 250 natural numbers (starting from 1). *)
let prog3 : Ast.prog =
    [
        lui r1 0;
        lli r1 250;
        addi r2 r0 0;

        beq r1 r0 4;
        add r2 r2 r1;
        addi r1 r1 (-1);
        j (-3);
    ]


let final = Proc.execute_prog prog3

let () = print_endline ""
let () = Rf.print (Proc.get_regfile final)


