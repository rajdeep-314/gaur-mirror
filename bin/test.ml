let () = print_endline "CPU simulator testing.\n"


module Proc = Cpusim.Proc
open Syntax.Ast.Helper
open Syntax.Ast.Reg.Helper


(* Testing the debug mode. *)

let prog = [
        lui r1 0;
        lli r1 5;
        lui r2 0xc0;
        lli r2 0x01;
        addi r3 r0 1;

        blt r1 r3 4;
        st r2 r3 0;
        addi r3 r3 1;
        j (-3);
]

let proc = Proc.create ()

let () = Debug.Repl.repl prog proc true

