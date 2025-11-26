open Assembler.Types
open Assembler.Assemble
open Assembler.Helpers
open Assembler.Printers


let p : string asm_prog =
    [
        label "x";
        lli r1 10;
        lli r2 20;
        label "y";
        addi r1 r0 0;
        add r2 r1 r1;
        label "meow";
        j "y";
    ]

let mp : (machine_prog, string) Result.t = assemble_prog p

let () =
    match mp with
    | Ok mp     -> print_endline (machine_prog_to_bin mp)
    | Error e   -> print_endline e

