(*
  disassembler_web.ml

  Browser interface for the Disassembler.
*)

open Js_of_ocaml

module D = Dis.Decode
module IP = Types.Isa.Printers
module EP = Dis.Errors.Printers
module E  = Dis.Errors

module DU = Dom_utils
module M = Misc

(* Read the selected browser file and disassemble it. *)
let disassemble_file
    file
    result
    errors
    current_output
    current_filename =

  let reader = new%js File.fileReader
  in
  reader##.onload :=
    Dom.handler (fun _ ->
      (match
        Js.Opt.to_option (File.CoerceTo.arrayBuffer reader##.result)
      with

      | None ->
          DU.set_text errors "Unable to read file as an ArrayBuffer."

      | Some buffer ->
          begin
            match M.words_of_array_buffer buffer with
            | Error size ->
                DU.set_text errors (EP.err (E.InvalidFileSize size))

            | Ok instrs ->
                begin
                  match D.disassemble_prog instrs with
                  | Error e ->
                      DU.set_text errors (EP.err e)

                  | Ok p ->
                      let output = IP.prog p in
                      current_output := Some output;
                      DU.set_text result output
                end
          end);

      Js._false
    );

  reader##readAsArrayBuffer file


let () =

  let input_file = DU.get_input "dis-input-file" in
  let button = DU.get_element "disassemble-button" in
  let result = DU.get_element "disassembler-result" in
  let errors = DU.get_element "disassembler-errors" in
  let copy_output = DU.get_element "copy-output" in
  let copy_errors = DU.get_element "copy-errors" in
  let download_output = DU.get_element "download-output" in

  let current_output = ref None in
  let current_filename = ref "disassembled.gsm.inter" in

  DU.setup_copy_button copy_output result;
  DU.setup_copy_button copy_errors errors;

  DU.setup_download_button
    download_output
    current_output
    current_filename
    errors
    M.download_file;

  DU.clear_text result;
  DU.clear_text errors;

  button##.onclick :=
    Dom_html.handler (fun _ ->
      DU.clear_text result;
      DU.clear_text errors;
      current_output := None;

      (match Js.Optdef.to_option input_file##.files with
      | None ->
          DU.set_text errors "No file selected."

      | Some files ->
          if files##.length = 0 then
            DU.set_text errors "No file selected."
          else
            match Js.Opt.to_option (files##item 0) with
            | None ->
                DU.set_text errors "Unable to access selected file."

            | Some file ->
                let input_filename = Js.to_string file##.name in
                let output_filename =
                  M.swap_suffix input_filename ".gsm.inter"
                in
                current_filename := output_filename;

                disassemble_file file result errors current_output current_filename
        );

      Js._false
    )
