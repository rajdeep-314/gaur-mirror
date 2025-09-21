let () = print_endline "CA lab 1 demo\n"

module Ast = Cpusim.Ast
module Ram = Cpusim.Ram
module Rf = Cpusim.Regfile
module Proc = Cpusim.Proc

open Ast.Reg.Helper
open Ast.Helper


(*
1. r10 <- 0x34ab

    Program:
        li r10 0x34ab
*)
let p1 : Ast.prog =
    [
        lui r10 0x34;
        lli r10 0xab;
    ]

let () = print_endline "\nProgram 1:\n\tr10 <- 0x34ab.\n"
let _final = Proc.execute_prog_pt p1


(*
2. RAM[3] <- RAM[1] + RAM[2]

    Program:
        ld r1 r0 1
        ld r2 r0 2
        add r1 r1 r2
        st r1 r0 3
*)
let p2 : Ast.prog =
    [
        ld r1 r0 1;
        ld r2 r0 2;
        add r1 r1 r2;
        st r1 r0 3;
    ]

let init_ram = Ram.init_from_mmap [(1, 32); (2, 10)]

let () = print_endline "\n\nProgram 2:\n\tRAM[3] <- RAM[1] + RAM[2].\n"
let _final = Proc.execute_prog_on_ram_pt init_ram p2


(*
3. Set RAM[1] to RAM[5] to 0xc001.

    Program:
        li r1 5
        li r2 0xc001
        li r3 1

        loop:
        blt r1 r3 end
        st r2 r3 0
        j loop

        end:
*)
let p3 : Ast.prog =
    [
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

let () = print_endline "\n\nProgram 3:\n\tSet RAM[1] to RAM[5] to 0xc001.\n"
let _final = Proc.execute_prog_pt p3


(*
6. Generate the Fibonacci sequence and store it in the RAM.
   Let RAM[0] and RAM[1] store n and base, respectively. The program should
   populate the RAM from addresses base through base + n - 1 with the first n
   elements of the Fibonacci sequence (0, 1, 1, 2, ...).

    Program:
        ld r1 r0 0
        ld r2 r0 1
        li r3 0
        li r5 0
        li r6 1

        loop:
        beq r3 r1 end
        add r4 r2 r3
        st r5 r4 0
        add r7 r5 r6
        mv r5 r6
        mv r6 r7
        addi r3 r3 1
        j loop

        end:
*)
let p6 : Ast.prog =
    [
        ld r1 r0 0;
        ld r2 r0 1;
        addi r3 r0 0;
        addi r5 r0 0;
        addi r6 r0 1;

        beq r3 r1 2;
        j 2;
        j 8;
        add r4 r2 r3;
        st r5 r4 0;
        add r7 r5 r6;
        addi r5 r6 0;
        addi r6 r7 0;
        addi r3 r3 1;
        j (-9);
    ]

let () = print_endline "\n\nProgram 6:\n\tGenerate the Fibonacci sequence.\n"

let init_ram = Ram.init_from_mmap [(0, 10); (1, 10)]
let _final = Proc.execute_prog_on_ram_pt init_ram p6

