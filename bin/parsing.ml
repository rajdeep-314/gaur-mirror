open Cpusim.Ast.Helper
open Cpusim.Ast.Reg.Helper

let () = print_endline "CPU simulator: Parser testing.\n"


let ap = [
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


let sp =
        "lui r1 0
        lli r1 5
        lui r2 192
        lli r2 1
        addi r3 r0 1
        blt r1 r3 4
        st r2 r3 0
        addi r3 r3 1
        j -3"


let p = Cpusim.Parser.parse_prog sp

let () = Cpusim.Ast.print_prog p
let () = print_endline "\n-----\n"
let () = Cpusim.Ast.print_prog ap

