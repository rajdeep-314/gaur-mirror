(* gen_bin.ml *)

(* This script is used to generate binaries  *)

let () =
  if Array.length Sys.argv <> 3 then
    begin
    prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " <opcode> <offset>");
    exit 1;
    end;

  let opcode =
    try int_of_string Sys.argv.(1)
    with Failure _ ->
      prerr_endline "Error: invalid opcode";
      exit 1
  in

  let offset =
    try int_of_string Sys.argv.(2)
    with Failure _ ->
      prerr_endline "Error: invalid offset";
      exit 1
  in

  if opcode < 0 || opcode > 0xFFFF then begin
    prerr_endline "Error: opcode must be a 16-bit value";
    exit 1
  end;

  if offset < 0 || offset > 0xFFFF then begin
    prerr_endline "Error: offset must be a 16-bit value";
    exit 1
  end;

  let instruction = (opcode lsl 16) lor offset in

  let b0 = (instruction lsr 24) land 0xFF in
  let b1 = (instruction lsr 16) land 0xFF in
  let b2 = (instruction lsr 8) land 0xFF in
  let b3 = instruction land 0xFF in

  Printf.printf "%c%c%c%c" (Char.chr b0) (Char.chr b1)
    (Char.chr b2) (Char.chr b3)