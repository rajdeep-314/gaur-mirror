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
module DU = Dom_utils


(** [parse_source src] parses string program [src] and returns a value in the
    type [(string prog, string] result]. The return value being [Ok p]
    denotes that there were no lexing or parsing errors, and the resultant
    AST program is [p]. [Error e] denotes that there was some error, the
    string representation of which is [e]. *)
let parse_source source =
    try
        let prog_ast =
        Parser.parse_prog_with_path "<web-input>" source in
        Ok (Types.remove_annot_from_prog prog_ast)
    with
    | Errors.LexingExn err ->
        Error (Errors.LexingErrors.to_string_with_src err source)
    | Errors.ParsingExn err ->
        Error (Errors.ParsingErrors.to_string_with_src err source)


(** [assemble_source src] assembles string source program [src] and returns [Ok
    p], [p] being the assembled prorogram, if there were no errors in lexing,
    parsing, or assembling, and it returns [Error e], with [e] being the
    error string -- which can arise from lexing errors, parsing errors, or
    assembly errors. *)
let assemble_source source =
    match parse_source source with
    | Error e -> Error e
    | Ok p    ->
        match Assemble.assemble_prog p with
        | Ok assembled_p -> Ok assembled_p
        | Error e        -> Error e

(** [assemble_to_inter src] is the variant of [assemble_source src] that does
    not go all the way through with the assembly and returns the intermediate
    form, with labels resolved. *)
let assemble_to_inter source =
    match parse_source source with
    | Error e -> Error e
    | Ok p    ->
        match Assemble.to_inter p with
        | Ok assembled_p -> Ok assembled_p
        | Error e        -> Error e

let perform_operation result errors current_output current_filename
    filename operation source =

    DU.clear_text result;
    DU.clear_text errors;

    begin
        match operation source with

        | Error e ->
            current_output := None;
            DU.set_text errors e

        | Ok (download_content, display_content) ->
            current_output := Some download_content;
            current_filename := filename;
            DU.set_text result display_content
    end

let output_filename input_filename format =
    match format with
    | "bin" ->
        Misc.swap_suffix input_filename ".gin"

    | "ascii" ->
        Misc.swap_suffix input_filename ".gsc"

    | "inter" ->
        Misc.swap_suffix input_filename ".gsm.inter"

    | _ ->
        raise (Invalid_argument "unknown format")

(* Common code for format buttons *)
let handle_format_button button result errors current_output current_filename
    last_operation file_input format default_filename operation =

    button##.onclick :=
        Dom_html.handler (fun _ ->

        last_operation := (operation, format, default_filename);

        let source =
            DU.get_textarea_value "assembly-input"
        in

        let filename =
            match DU.get_file_input_name file_input with
            | Some input_filename ->
                output_filename input_filename format

            | None ->
                default_filename
        in

        perform_operation result errors current_output
            current_filename filename operation source;

        Js._false
        )

let handle_editor_shortcuts input result errors current_output
    current_filename last_operation file_input
    (ev : Dom_html.keyboardEvent Js.t) =

  if ev##.keyCode = 13 &&
     Js.to_bool ev##.ctrlKey then begin

    Dom.preventDefault ev;

    if Js.to_bool ev##.shiftKey then begin

      match !current_output with
      | None ->
          DU.set_text errors "Nothing to download."

      | Some output ->
          Misc.download_file
            !current_filename
            output

    end else begin

      let source =
        DU.get_textarea_value "assembly-input"
      in

      let operation, format, default_filename =
        !last_operation
      in

      let filename =
        match DU.get_file_input_name file_input with
        | Some input_filename ->
            output_filename input_filename format

        | None ->
            default_filename
      in

      perform_operation result errors current_output
        current_filename filename operation source

    end;

    Js._false

  end else begin
    Js._true
  end


let () =

    let input = DU.get_textarea "assembly-input" in
    let line_numbers = DU.get_element "line-numbers" in
    let result = DU.get_element "assembly-result" in
    let errors = DU.get_element "assembly-errors" in

    let fmt_ascii = DU.get_element "format-ascii" in
    let fmt_bin = DU.get_element "format-bin" in
    let fmt_inter = DU.get_element "format-inter" in

    let copy_output = DU.get_element "copy-output" in
    let copy_errors = DU.get_element "copy-errors" in

    let assembly_file = DU.get_input "assembly-file" in
    let download_output = DU.get_element "download-output" in

    let ascii_operation source =
        match assemble_source source with
        | Error e ->
            Error e
        | Ok mp ->
            let output = MacPrinters.ascii_prog mp in
            Ok (output, output)
        in

    let binary_operation source =
        match assemble_source source with
        | Error e ->
            Error e
        | Ok mp ->
            let binary = MacPrinters.bin_prog mp in
            Ok (binary, Misc.hex_dump binary)
        in

    let inter_operation source =
        match assemble_to_inter source with
        | Error e ->
            Error e
        | Ok mp ->
            let output = IsaPrinters.prog mp in
            Ok (output, output)
        in

    let last_operation =
        ref (ascii_operation, "ascii", "assembled.gsc") in

    let current_output = ref None in
    let current_filename = ref "assembled_ascii.txt" in

    DU.setup_line_numbers_with_shortcuts input line_numbers
        (handle_editor_shortcuts input result errors
        current_output current_filename last_operation
        assembly_file);

    DU.setup_copy_button copy_output result;
    DU.setup_copy_button copy_errors errors;

    DU.set_text result "Waiting for input...";
    DU.clear_text errors;

    DU.setup_file_input assembly_file input line_numbers errors;

    handle_format_button
        fmt_ascii
        result
        errors
        current_output
        current_filename
        last_operation
        assembly_file
        "ascii"
        "assembled.gsc"
        ascii_operation;

    handle_format_button
        fmt_bin
        result
        errors
        current_output
        current_filename
        last_operation
        assembly_file
        "bin"
        "assembled.bin"
        binary_operation;

    handle_format_button
        fmt_inter
        result
        errors
        current_output
        current_filename
        last_operation
        assembly_file
        "inter"
        "assembled.gsm.inter"
        inter_operation;

    DU.setup_download_button download_output current_output
    current_filename errors Misc.download_file