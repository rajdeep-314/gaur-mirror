(* arbitary_binary.ml *)

(* The script generate n bytes binary with all bits 0. *)

let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " <size>");
    exit 1
  end;

  let size = int_of_string Sys.argv.(1) in

  for _ = 1 to size do
    output_char stdout '\x00'
  done