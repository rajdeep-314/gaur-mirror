open Big_int
open Raminit


let path = ref ""
let out_path = ref ""
let ram_size = ref 16384
let word_size = ref 2
let endianness = ref "big"
let offset = ref 0
let padding = ref "0"


let speclist = [
    ("--out", Arg.Set_string out_path, "Path of the output file, if any.");
    ("-o", Arg.Set_string out_path, "Path of the output file, if any (short).");
    ("-rsz", Arg.Set_int ram_size, "The RAM size, in number of words/cells (short).");
    ("--ram-size", Arg.Set_int ram_size, "The RAM size, in number of words/cells.");
    ("-rsz", Arg.Set_int ram_size, "The RAM size, in number of words/cells (short).");
    ("--word-size", Arg.Set_int word_size, "The word size, in number of bytes.");
    ("-wsz", Arg.Set_int word_size, "The word size, in number of bytes (short).");
    ("--endianness", Arg.Set_string endianness, "The endianness (little/big).");
    ("-e", Arg.Set_string endianness, "The endianness (little/big) (short).");
    ("--offset ", Arg.Set_int offset, "The address offset for the RAM.");
    ("-off", Arg.Set_int offset, "The address offset for the RAM (short).");
    ("--padding", Arg.Set_string padding, "Data to be loaded at unspecified addresses.");
    ("-p", Arg.Set_string padding, "Data to be loaded at unspecified addresses (short).");
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
    if !path == "" then
        begin
        print_endline "Error: File path not passed as argument.";
        Arg.usage speclist usage_msg;
        exit 2;
        end

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

let ms = Parser.parse_maps maps

let bs = Translate.translate_maps ms !ram_size !word_size lendianb !offset (big_int_of_string !padding)

let () =
    if !out_path = "" then
        Binary.print bs
    else
        let oc = open_out !out_path in
        Fun.protect
            ~finally:(fun () -> close_out_noerr oc)
            (fun () -> output_string oc (Binary.to_string bs))

