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



(* Common code for format buttons *)
let handle_format_button button result errors operation =
  button##.onclick :=
    Dom_html.handler (fun _ ->
      let source =
        WU.get_textarea_value "assembly-input"
      in

      WU.clear_text result;
      WU.clear_text errors;

      begin
      match operation source with
      | Error e   -> WU.set_text errors e
      | Ok output -> WU.set_text result output
      end;
      Js._false
    )


let () =

  let input = WU.get_textarea "assembly-input" in
  let line_numbers = WU.get_element "line-numbers" in
  let result = WU.get_element "assembly-result" in
  let errors = WU.get_element "assembly-errors" in
  let fmt_ascii = WU.get_element "format-ascii" in
  let fmt_bin = WU.get_element "format-bin" in
  let fmt_inter = WU.get_element "format-inter" in

  WU.setup_line_numbers input line_numbers;

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
      match assemble_to_inter source with
      | Error e ->
          Error e
      | Ok mp ->
          Ok (IsaPrinters.prog mp)
    )

