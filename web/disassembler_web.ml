(*
  disassembler_web.ml

  Browser interface for the Disassembler.
*)

open Js_of_ocaml

module D = Dis.Decode
module IP = Types.Isa.Printers
module EP = Dis.Errors.Printers
module E  = Dis.Errors

module WU = Web_utils
module M = Misc

(* Read the selected browser file and disassemble it. *)
let disassemble_file file result errors =

  let reader = new%js File.fileReader
  in
  reader##.onload :=
    Dom.handler (fun _ ->
      (match
         Js.Opt.to_option (File.CoerceTo.arrayBuffer reader##.result)
      with

      | None ->
          WU.set_text errors "Unable to read file as an ArrayBuffer."

      | Some buffer ->

          begin
            match M.words_of_array_buffer buffer with
            | Error size ->
                WU.set_text errors (EP.err (E.InvalidFileSize size))
            | Ok instrs ->
                begin
                  match D.disassemble_prog instrs with
                  | Error e ->
                      WU.set_text errors (EP.err e)
                  | Ok p ->
                      WU.set_text result (IP.prog p)
                end
          end);

      Js._false
    );

  reader##readAsArrayBuffer file


let () =

  let input_file = WU.get_input "dis-input-file" in
  let button = WU.get_element "disassemble-button" in
  let result = WU.get_element "disassembler-result" in
  let errors = WU.get_element "disassembler-errors" in

  WU.clear_text result;
  WU.clear_text errors;

  button##.onclick :=
    Dom_html.handler (fun _ ->
      WU.clear_text result;
      WU.clear_text errors;
      (match Js.Opt.to_option input_file##.files with
        | None ->
            WU.set_text errors "No file selected."

        | Some files ->
            if files##.length = 0 then
              WU.set_text errors "No file selected."
            else
              match Js.Opt.to_option (files##item 0) with
              | None ->
                  WU.set_text errors "Unable to access selected file."
              | Some file ->
                  disassemble_file file result errors);

      Js._false
    )