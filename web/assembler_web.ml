(*
  assembler_web.ml

  Browser interface for the Assembler.
*)

open Js_of_ocaml

module Assemble = Assembler.Assemble
module Parser = Parsing.Asm.Driver
module IsaPrinters = Types.Isa.Printers
module MacPrinters = Types.Machine.Printers
module Types = Types.Asm
module Errors = Parsing.Asm.Errors
module WU = Web_utils


(* Parse the source program *)
let parse_source source =
  let prog_ast =
    Parser.parse_prog_with_path "<web-input>" source
  in
  Types.remove_annot_from_prog prog_ast


(* Parse and assemble the source program *)
let assemble_source source =
  let prog_ast =
    parse_source source
  in
  Assemble.assemble_prog prog_ast


(* Update the line numbers next to the assembly editor *)
let update_line_numbers textarea line_numbers =

  let source = Js.to_string textarea##.value in
  let line_count =  List.length (String.split_on_char '\n' source) in
  let numbers = String.concat "\n" (List.init line_count (fun i -> string_of_int (i + 1))) in
  line_numbers##.textContent := Js.some (Js.string numbers)


(* Set up line numbers and scrolling for the editor *)
let setup_line_numbers () =
  let textarea =  WU.get_textarea "assembly-input" in
  let line_numbers = WU.get_element "line-numbers"in

  update_line_numbers textarea line_numbers;

  (* Update line numbers whenever text changes *)
  textarea##.oninput :=
    Dom_html.handler (fun _ ->
      update_line_numbers textarea line_numbers;
      Js._false
    );

  (* Keep line numbers aligned while scrolling *)
  textarea##.onscroll :=
    Dom_html.handler (fun _ ->
      line_numbers##.scrollTop :=
        textarea##.scrollTop;

      Js._false
    )


(* Common code for format buttons *)
let handle_format_button button result errors operation =
  button##.onclick :=
    Dom_html.handler (fun _ ->
      let source =
        WU.get_textarea_value "assembly-input"
      in

      WU.clear_text result;
      WU.clear_text errors;

      try
        (match operation source with
         | Error e ->
             WU.set_text errors e
         | Ok output ->
             WU.set_text result output);

        Js._false

      with
        | Errors.LexingExn err ->
            WU.set_text
              errors
              (Errors.LexingErrors.to_string_with_src err source);

            Js._false

        | Errors.ParsingExn err ->
            WU.set_text
              errors
              (Errors.ParsingErrors.to_string_with_src err source);

            Js._false
    )


let () =

  let result = WU.get_element "assembly-result" in
  let errors = WU.get_element "assembly-errors" in
  let fmt_ascii = WU.get_element "format-ascii" in
  let fmt_bin = WU.get_element "format-bin" in
  let fmt_inter = WU.get_element "format-inter" in

  setup_line_numbers ();

  WU.set_text result "Waiting for input...";
  WU.clear_text errors;

  (* Ascii *)
  handle_format_button fmt_ascii result errors
    (fun source ->
      match assemble_source source with
      | Error e ->
          Error e
      | Ok mp ->
          Ok (MacPrinters.ascii_prog mp)
    );


  (* Binary *)
  handle_format_button fmt_bin result errors
    (fun source ->
      match assemble_source source with
      | Error e ->
          Error e
      | Ok mp ->
          Ok (MacPrinters.bin_prog mp)
    );


  (* Intermediate *)
  handle_format_button fmt_inter result errors
    (fun source ->
      let prog_ast =
        parse_source source
      in

      match Assemble.to_inter prog_ast with
      | Error e ->
          Error e
      | Ok inter ->
          Ok (IsaPrinters.prog inter)
    )