open Assembler.Types
open Assembler.Assemble
open Assembler.Printers
open Assembler.Parser

let p : string =
  "x:
      lli r1 10     # xyz
      lli r2 20     # abcd
        y:
    addi r1 r0 0
    add r2 r1 r1

      meow:
            j y

    # Ending comments.
  "

let p = parse_prog p

let inter : (isa_prog, string) Result.t = to_inter p
let mp : (machine_prog, string) Result.t = assemble_prog p

let () = print_endline "Intermediate:"
let () =
    match inter with
    | Ok im     -> print_endline (isa_prog_to_string im)
    | Error e   -> print_endline ("Error: " ^ e)


let () = print_endline "\nAssembled (ASCII):"
let () =
    match mp with
    | Ok mp     -> print_endline (machine_prog_to_ascii mp)
    | Error e   -> print_endline ("Error: " ^ e)

