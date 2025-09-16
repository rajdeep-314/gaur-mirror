let () = print_endline "CPU simulator for PISA."


(* Testing. *)

module Proc = Cpusim.Proc
module Ast = Cpusim.Ast
module Rf = Cpusim.Regfile

open Ast.Reg.Helper
open Ast.Helper

let prog : Ast.prog =
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

let final = Proc.execute_prog prog

let () = print_endline ""
let () = Rf.print (Proc.get_regfile final)


