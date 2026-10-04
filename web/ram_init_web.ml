(*
  ram_init_web.ml

  Browser interface for the RAM initializer.
*)
open Js_of_ocaml
open Big_int

module Parse = Raminit.Parse
module Translate = Raminit.Translate
module Binary = Raminit.Binary

module DU = Dom_utils
module M = Misc

(*    Process selected memory-map file   *)
(* Process a memory-map source *)
let process_source source status hex errors status_section hex_section
    error_section download_section current_output current_filename
    ram_size word_size lendianb offset padding output_file =

  try
    let maps = Parse.parse_maps source in

    DU.set_text status
      "Memory map parsed. Translating...";

    (* Generate RAM contents *)
    let bs =
      Translate.translate_maps
        maps
        ram_size
        word_size
        lendianb
        offset
        padding
    in

    let binary = Binary.to_string bs in

    DU.set_text hex (M.hex_dump binary);

    let filename =
      if output_file = "" then
        "ram.bin"
      else
        output_file
    in

    (* Store result for explicit download *)
    current_output := Some binary;
    current_filename := filename;

    DU.show_element status_section;
    DU.show_element hex_section;
    DU.hide_element error_section;
    DU.show_element download_section;

    DU.set_text status
      ("RAM initialization successful.\n"
       ^ "Output file: " ^ filename ^ "\n"
       ^ "Output size: "
       ^ string_of_int (String.length binary)
       ^ " bytes")

  with

  | Failure msg ->
    current_output := None;
    DU.hide_element status_section;
    DU.hide_element hex_section;
    DU.hide_element download_section;
    DU.show_element error_section;
    DU.set_text errors msg

  | Invalid_argument msg ->
    current_output := None;
    DU.hide_element status_section;
    DU.hide_element hex_section;
    DU.hide_element download_section;
    DU.show_element error_section;
    DU.set_text errors msg

  | exn ->
    current_output := None;

    DU.hide_element status_section;
    DU.hide_element hex_section;
    DU.hide_element download_section;
    DU.show_element error_section;

    let message =
      match Printexc.to_string exn with
      | "Raminit.Parser.MenhirBasics.Error" ->
          "Invalid memory map format.\n\
           Expected entries in the form:\n\
           <address> : <value>"

      | msg ->
          msg
    in

    DU.set_text errors message

let () =

  let file_input = DU.get_input "ram-map-file" in
  let map_input = DU.get_textarea "ram-map-input" in

  let button = DU.get_element "ram-init-button" in
  let download_section = DU.get_element "download-section" in
  let download_output = DU.get_element "download-output" in

  let status_section = DU.get_element "status-section" in
  let hex_section = DU.get_element "hex-section" in
  let error_section = DU.get_element "error-section" in

  let status = DU.get_element "ram-status" in
  let hex = DU.get_element "ram-hex" in
  let errors = DU.get_element "ram-errors" in

  let copy_status = DU.get_element "copy-status" in
  let copy_hex = DU.get_element "copy-hex" in
  let copy_errors = DU.get_element "copy-errors" in

  let current_output = ref None in
  let current_filename = ref "ram.bin" in

  DU.setup_copy_button copy_status status;
  DU.setup_copy_button copy_hex hex;
  DU.setup_copy_button copy_errors errors;

  DU.setup_download_button download_output current_output current_filename errors M.download_file;

  DU.hide_element status_section;
  DU.hide_element hex_section;
  DU.hide_element error_section;
  DU.hide_element download_section;

  DU.clear_text status;
  DU.clear_text hex;
  DU.clear_text errors;

  file_input##.onchange :=
  Dom_html.handler (fun _ ->

    begin
      match Js.Opt.to_option file_input##.files with

      | None -> ()

      | Some files ->
          if files##.length = 0 then ()

          else
            begin
              match Js.Opt.to_option (files##item 0) with

              | None ->
                  DU.set_text errors "Unable to access selected file."
              | Some file ->
                  DU.read_file_as_text file (function
                    | None ->
                        DU.set_text errors "Unable to read memory map file."
                    | Some source ->
                        map_input##.value := Js.string source
                  )
            end
    end;

    Js._false
  );

  button##.onclick :=
  Dom_html.handler (fun _ ->

    DU.clear_text status;
    DU.clear_text hex;
    DU.clear_text errors;

    current_output := None;

    DU.hide_element status_section;
    DU.hide_element hex_section;
    DU.hide_element error_section;
    DU.hide_element download_section;

    begin
      try
        let source = DU.get_textarea_value "ram-map-input" in
        let ram_size = int_of_string (DU.get_input_value "ram-size") in
        let word_size = int_of_string (DU.get_input_value "word-size") in
        let endianness = DU.get_select_value "endianness" in
        let lendianb = if endianness = "little" then true else false in
        let offset = int_of_string (DU.get_input_value "offset") in
        let padding = big_int_of_string (DU.get_input_value "padding") in
        let output_file = DU.get_input_value "output-file" in
        if String.trim source = "" then begin
            DU.hide_element status_section;
            DU.hide_element hex_section;
            DU.hide_element download_section;
            DU.show_element error_section;

            DU.set_text errors "No memory map provided."
            end

        else
          process_source source status hex errors status_section hex_section
            error_section download_section current_output current_filename
            ram_size word_size lendianb offset padding output_file

      with

      | Failure msg ->
          DU.set_text errors msg

      | Invalid_argument msg ->
          DU.set_text errors msg

      | exn ->
          let message =
            match Printexc.to_string exn with
            | "Raminit.Parser.MenhirBasics.Error" ->
                "Invalid memory map format.\n\
                Expected entries in the form:\n\
                <address> : <value>"

            | msg ->
                msg
          in
          DU.set_text errors message
          end;

    Js._false
  )
