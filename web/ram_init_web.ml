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
let process_file file status errors ram_size word_size lendianb offset padding output_file =

  let reader =
    new%js File.fileReader
  in

  reader##.onload :=
    Dom.handler (fun _ ->

    (match Js.Opt.to_option (File.CoerceTo.string reader##.result) with
      | None ->
          DU.set_text errors "Unable to read memory map file."

      | Some js_result ->
          let source = Js.to_string js_result in

          begin
            try
              let maps = Parse.parse_maps source in
                DU.set_text status "Memory map parsed. Translating...";

              (* Generate RAM contents *)
              let bs = Translate.translate_maps maps ram_size word_size lendianb offset padding in
              let binary = Binary.to_string bs in
              let filename = if output_file = "" then "ram.bin" else output_file in

              (* Download result *)
              M.download_file filename binary;

              DU.set_text status
                ("RAM initialization successful.\n"
                 ^ "Output file: " ^ filename ^ "\n"
                 ^ "Output size: " ^ string_of_int (String.length binary) ^ " bytes")
            with

            | Failure msg ->
                DU.set_text errors msg

            | Invalid_argument msg ->
                DU.set_text errors msg

          end);

      Js._false
    );

  reader##readAsText file


let () =

  let file_input = DU.get_input "ram-map-file" in
  let button = DU.get_element "ram-init-button" in
  let status = DU.get_element "ram-status" in
  let errors = DU.get_element "ram-errors" in

  DU.clear_text status;
  DU.clear_text errors;

  button##.onclick :=
    Dom_html.handler (fun _ ->

      DU.clear_text status;
      DU.clear_text errors;

      (match Js.Opt.to_option file_input##.files with
        | None -> DU.set_text errors "No file selected."

        | Some files ->
            if files##.length = 0 then DU.set_text errors "No file selected."

            else
              match Js.Opt.to_option (files##item 0) with

              | None -> DU.set_text errors "Unable to access selected file."

              | Some file ->
                  begin
                    try

                      let ram_size = int_of_string (DU.get_input_value "ram-size") in
                      let word_size = int_of_string (DU.get_input_value "word-size") in
                      let endianness = (DU.get_select_value "endianness") in
                      let lendianb = if endianness = "little" then true else false in
                      let offset = int_of_string (DU.get_input_value "offset") in
                      let padding = big_int_of_string (DU.get_input_value "padding") in
                      let output_file = DU.get_input_value "output-file" in
                      DU.set_text status "Reading memory map...";

                      process_file file status errors ram_size word_size lendianb offset padding output_file

                    with

                    | Failure msg ->
                        DU.set_text errors msg

                    | Invalid_argument msg ->
                        DU.set_text errors msg

                  end);

      Js._false
    )
