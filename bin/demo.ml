let () = print_endline "CA lab 1 demo\n"

module Ast = Syntax.Ast
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
let final = Proc.execute_prog p1

let () = Rf.print (Proc.get_regfile final)


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
let final = Proc.execute_prog_on_ram init_ram p2

let () = Ram.print_all_nz (Proc.get_ram final)


(*
3. Set RAM[1] to RAM[25] to 0xc001.

    Program:
        li r1 25
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
        lli r1 25;
        lui r2 0xc0;
        lli r2 0x01;
        addi r3 r0 1;

        blt r1 r3 4;
        st r2 r3 0;
        addi r3 r3 1;
        j (-3);
    ]

let () = print_endline "\n\nProgram 3:\n\tSet RAM[1] to RAM[25] to 0xc001.\n"
let final = Proc.execute_prog p3

let () = Ram.print_all_nz (Proc.get_ram final)


(*
4. Let the base address and length of an array of half words be present in
   RAM[1] and RAM[0], respectively. Count the number of non-negative, even numbers
   in the array and store the count in r10.

    Program:
        ld r1 r0 0
        ld r2 r0 1
        li r10 0
        li r3 0
        li r5 1

        loop:
        beq r3 r1 end
        add r4 r2 r3
        ld r4 r4 0
        blt r4 r0 next
        and r4 r4 r5
        beq r4 r5 next
        addi r10 r10 1

        next:
        addi r3 r3 1
        j loop

        end:
*)
let p4 : Ast.prog =
    [
        ld r1 r0 0;
        ld r2 r0 1;
        addi r10 r0 0;
        addi r3 r0 0;
        addi r5 r0 1;

        beq r3 r1 2;
        j 2;
        j 9;
        add r4 r2 r3;
        ld r4 r4 0;
        blt r4 r0 4;
        and_ r4 r4 r5;
        beq r4 r5 2;
        addi r10 r10 1;

        addi r3 r3 1;
        j (-10);
    ]

let init_ram = Ram.init_from_mmap [(0, 5); (1, 10); (10, 2); (11, -1); (12, 7); (13, 5); (14, 4)]

let () = print_endline "\n\nProgram 4:\n\tCount non-negative even numbers in an array and store the count in r10.\n"
let final = Proc.execute_prog_on_ram init_ram p4

let () = Rf.print (Proc.get_regfile final)

let () = print_endline ""
let () = Ram.print_all_nz (Proc.get_ram final)


(*
5. Let the base address and length of an array of half words be present in
   RAM[0] and RAM[1], respectively. Sort the array in a non-decreasing order,
   in-place.

    Program:
        ld r1 r0 0
        ld r2 r0 1
        li r3 0

        loop:
        beq r3 r2 end
        add r4 r1 r3
        ld r5 r4 0
        ld r6 r4 1
        blt r5 r6 next
        st r6 r4 0
        st r5 r4 1
        mv r6 r7
        li r3 0
        j loop

        next:
        addi r3 r3 1
        j loop

        end:
*)
let p5 : Ast.prog =
    [
        ld r1 r0 0;
        ld r2 r0 1;
        addi r3 r0 0;
        addi r2 r2 (-1);

        beq r3 r2 2;
        j 2;
        j 12;
        add r4 r1 r3;
        ld r5 r4 0;
        ld r6 r4 1;
        blt r5 r6 6;
        st r6 r4 0;
        st r5 r4 1;
        addi r6 r7 0;
        addi r3 r0 0;
        j (-11);
        addi r3 r3 1;
        j (-13);
    ]

let init_ram = Ram.init_from_mmap [(0, 10); (1, 8); (10, 70); (11, 80); (12, 40); (13, 20); (14, 10); (15, 30); (16, 50); (17, 60)]

let () = print_endline "\n\nProgram 5:\n\tSort an array in-place.\n"

let initial = Proc.execute_prog_on_ram init_ram []
let () = print_endline "Initial RAM:"
let () = Ram.print_all_nz (Proc.get_ram initial)

let final = Proc.execute_prog_on_ram init_ram p5
let () = print_endline "\nFinal RAM:"
let () = Ram.print_all_nz (Proc.get_ram final)


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

let init_ram = Ram.init_from_mmap [(0, 10); (1, 10)]

let () = print_endline "\n\nProgram 6:\n\tGenerate the Fibonacci sequence.\n"

let final = Proc.execute_prog_on_ram init_ram p6
let () = Ram.print_all_nz (Proc.get_ram final)

