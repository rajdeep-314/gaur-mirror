(*
    main.ml

    The top-level script.
*)


open Big_int
open Raminit

let path = ref ""               (* Input file path. *)
let out_path = ref ""           (* Output file path. *)
let ram_size = ref 16384        (* RAM size in bytes. *)
let word_size = ref 4           (* Word size in bytes. *)
let endianness = ref "big"      (* Endianness (little | big). *)
let offset = ref 0              (* Offset in number of bytes. *)
let padding = ref "0"           (* Padding bytes. *)
let help = ref false            (* Asking for help? *)


let speclist = [
    ("--out", Arg.Set_string out_path, "\n\tPath of the output file, if any") ;
    ("-o", Arg.Set_string out_path, "\n\tShort for --out") ;

    ("--ram-size", Arg.Set_int ram_size, "\n\tThe RAM size, in number of words/cells") ;
    ("-rsz", Arg.Set_int ram_size, "\n\tShort for --ram-size") ;

    ("--word-size", Arg.Set_int word_size, "\n\tThe word size, in number of bytes") ;
    ("-wsz", Arg.Set_int word_size, "\n\tShort for --word-size") ;

    ("--endianness", Arg.Set_string endianness, "\n\tThe endianness (little/big)") ;
    ("-e", Arg.Set_string endianness, "\n\tShort for --endianness") ;

    ("--offset", Arg.Set_int offset, "\n\tThe address offset for the RAM") ;
    ("-off", Arg.Set_int offset, "\n\tShort for --offset") ;

    ("--padding", Arg.Set_string padding, "\n\tData to be loaded at unspecified addresses") ;
    ("-p", Arg.Set_string padding, "\n\tShort for --padding") ;

    ( "--help", Arg.Set help, "\n\tDisplay this list of options" ) ;
    ( "-help", Arg.Set help, "\n\tDisplay this list of options" ) ;
    ( "-h", Arg.Set help, "\n\tShort for --help / -help" )
]


let usage_msg = "Usage: ram-init [options] <file_path>."

let anon_fun arg =
    if !path = "" then
        path := arg
    else
        begin
        print_endline "Error: Too many positional arguments (only one allowed).";
        Arg.usage speclist usage_msg;
        exit 1;
        end

let () =
    Arg.parse speclist anon_fun usage_msg

let () =
    if !help then
        begin
        Arg.usage speclist usage_msg ;
        exit 0
        end

let () =
    if !path == "" then
        begin
        print_endline "Error: File path not passed as argument.";
        Arg.usage speclist usage_msg;
        exit 2;
        end

(* Left endian? *)
let lendianb = 
    let endianness_lc = String.lowercase_ascii !endianness in
    if endianness_lc = "little" then true
    else if endianness_lc = "big" then false
    else
        begin
        print_endline "Error: Invalid endianness. Only little and big are supported right now.";
        exit 3;
        end

let maps =
    let inc = open_in !path in
    really_input_string inc (in_channel_length inc)

let ms = Parse.parse_maps maps

let bs = Translate.translate_maps ms !ram_size !word_size lendianb !offset (big_int_of_string !padding)

let () =
    if !out_path = "" then
        Binary.print bs
    else
        let oc = open_out !out_path in
        Fun.protect
            ~finally:(fun () -> close_out_noerr oc)
            (fun () -> output_string oc (Binary.to_string bs))

