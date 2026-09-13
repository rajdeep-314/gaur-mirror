(** immediate.ml *)

(**
Generates test cases for immediate arguments for various instructions.

Arguments:
    1. Instruction name ([inst])
    2. Immediate ([n])

Prints to stdout an assembly program with instruction [inst], with the
immediate argument being [n].
*)

let () =
  if Array.length Sys.argv <> 3 then
    begin
      prerr_endline "Usage: <inst> <imm>";
      exit 1;
    end

let inst = Sys.argv.(1)

let imm =
  match int_of_string_opt Sys.argv.(2) with
    | Some n -> string_of_int n
    | None   ->
        prerr_endline "Immediate must be an integer" ;
        exit 2

let out =
  match inst with
  | "li" -> "li r1 " ^ imm
  | "ld" -> "ld r1 r2 " ^ imm
  | "st" -> "st r1 r2 " ^ imm
  | "addi" -> "addi r1 r2 " ^ imm
  | "jalr" -> "jalr r1 r2 " ^ imm
  | "apc"  -> "apc r1 " ^ imm
  | _ ->
      begin
      prerr_endline "Invalid instruction";
      exit 3;
      end

let () = print_endline out
