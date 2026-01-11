let () = print_endline "Disassembler for PISA."

module Printers = Assembler.Printers
module Err = Dis.Err
module Decode = Dis.Decode
module Utils = Dis.Utils

let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "Usage: dispasm <binary>";
    exit 1
  end;
  let prog = Utils.read_program Sys.argv.(1) in
  match Decode.disassemble_prog prog with
  | Error e ->
      prerr_endline (Err.print_err e);
      exit 1
  | Ok insts ->
      Printers.print_isa_prog insts
