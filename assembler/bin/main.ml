(*
    main.ml

    The top-level script.
*)


(* Module aliases. *)
module Assemble       = Extracted.Assemble
module Parser         = Parsing.Asm.Driver
module Isa            = Extracted.Isa
module MacPrinters    = Extracted.Machine.Printers
module AssembleErrors = Extracted.Errors
module Errors         = Parsing.Asm.Errors


(* Exit codes. *)
let err_code_success        = 0     (* Exited successfully.           *)
let err_code_command_line   = 1     (* Command-line errors.           *)
let err_code_lexing_parsing = 2     (* Lexing or parsing errors.      *)
let err_code_assembling     = 3     (* Errors at the assembler level. *)


(* Parameters. *)

let path = ref ""               (* Input file path. *)
let format = ref "bin"          (* Output file format. *)
let out = ref ""                (* Output file path. *)
let help = ref false            (* Asking for help? *)

let speclist = [
    ("--format",Arg.Set_string format,"\n\tOutput format (bin | inter | ascii)");
    ("-fmt",Arg.Set_string format,"\n\tShort for --format");

    ("--out",Arg.Set_string out,"\n\tOutput file path");
    ("-o",Arg.Set_string out,"\n\tShort for --out") ;

    ( "--help", Arg.Set help, "\n\tDisplay this list of options" ) ;
    ( "-help", Arg.Set help, "\n\tDisplay this list of options" ) ;
    ( "-h", Arg.Set help, "\n\tShort for --help / -help" )
]

let usage_msg = "Usage: assembler [options] <file_path>.\n" ^
                "Exit codes:\n" ^
                "  0  Success (no errors)\n" ^
                "  1  Command-line errors\n" ^
                "  2  Lexing or parsing errors\n" ^
                "  3  Assembling errors"


let remove_extension filename =
    match String.rindex_opt filename '.' with
    | None      -> filename
    | Some idx  -> String.sub filename 0 idx

let default_output_name str =
    match !format with
    | "bin"     -> remove_extension str ^ ".gin"
    | "ascii"   -> remove_extension str ^ ".gsc"
    | "inter"   -> remove_extension str ^ ".gsm.inter"
    | _         -> raise (Invalid_argument "unknown format")


let anon_fun arg =
    if !path = "" then
        path := arg
    else
        begin
        prerr_endline "Error: Too many positional arguments (only one allowed).";
        Arg.usage speclist usage_msg;
        exit err_code_command_line;
        end

let () =
    Arg.parse speclist anon_fun usage_msg

(* Checking if help was asked for. *)
let () =
    if !help then
        begin
        Arg.usage speclist usage_msg ;
        exit err_code_success
        end

let () =
    if !path = "" then
    begin
    prerr_endline "Error: Missing input file path.";
    Arg.usage speclist usage_msg;
    exit err_code_command_line
    end

let src =
    try
        Utils.Files.read_file !path
    with
    | Sys_error e ->
        prerr_endline (Printf.sprintf "System error:\n%s" e);
        exit err_code_command_line

let prog_ast_annotated =
    try
        Parser.parse_prog_with_path !path src
    with
    | Errors.LexingExn err ->
        prerr_endline (Errors.LexingErrors.to_string_with_src err src);
        exit err_code_lexing_parsing
    | Errors.ParsingExn err ->
        prerr_endline (Errors.ParsingErrors.to_string_with_src err src);
        exit err_code_lexing_parsing

(** Intermediate (ISA) program. *)
let inter_prog = Assemble.asm_prog_to_isa_prog prog_ast_annotated

(** Machine program. *)
let mach_prog = Assemble.assemble prog_ast_annotated

let output_str =
    match mach_prog with
    | Error e   ->
            prerr_endline (AssembleErrors.to_string
                (fun annot -> Utils.Parsingerrs.pp_pos_info_with_source
                    ~source:src annot) e);
            exit err_code_assembling
    | Ok mp     ->
            match !format with
            | "ascii" -> MacPrinters.Ascii.prog mp ^ "\n"
            | "bin"   -> MacPrinters.Binary.prog mp
            | "inter" ->
                    begin
                    match inter_prog with
                    | Error e   ->
                            prerr_endline (AssembleErrors.to_string
                                (fun annot -> Utils.Parsingerrs.pp_pos_info_with_source
                                    ~source:src annot) e);
                            exit err_code_assembling
                    | Ok inter  ->
                            Isa.Printers.prog (Isa.prog_of_annot_prog inter) ^ "\n"
                    end
            | _       ->
                prerr_endline ("Unknown format: " ^ !format);
                exit err_code_command_line

let () =
    let outfile =
        if !out <> "" then !out else default_output_name !path in
    let oc = open_out outfile in
    output_string oc output_str;
    close_out oc;
    Printf.printf "Wrote: %s\n" outfile

