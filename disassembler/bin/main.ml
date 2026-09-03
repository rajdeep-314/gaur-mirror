(*
    main.ml
*)


module UF = Utils.Files
module D = Dis.Decode

module IP = Types.Isa.Printers
module EP = Dis.Errors.Printers
module E  = Dis.Errors


let input_path = ref ""
let output_path = ref ""
let help = ref false

let speclist = [
    ( "--out", Arg.Set_string output_path, "\n\tPath for the generated PASM output") ;
    ( "-o", Arg.Set_string output_path, "\n\tShort for --out") ;

    ( "--help", Arg.Set help, "\n\tDisplay this list of options" ) ;
    ( "-help", Arg.Set help, "\n\tDisplay this list of options" ) ;
    ( "-h", Arg.Set help, "\n\tShort for --help / -help" )
]

let usage_msg = "Usage: dis [options] <input_file_path>."

let anon_fun arg =
    if !input_path = "" then
        input_path := arg
    else
        begin
        print_endline "Top-level error: Only one positional argument expected." ;
        Arg.usage speclist usage_msg ;
        exit 1 ;
        end

let default_exten = ".pasm"

let default_output_path ip op =
    if op <> "" then op else
    let ip_no_exten =
        match String.rindex_opt ip '.' with
        | Some k    -> String.sub ip 0 k
        | None      -> ip
    in
    ip_no_exten ^ default_exten

(* Parsing command-line arguments. *)
let () =
    Arg.parse speclist anon_fun usage_msg

(* Checking if help was asked for. *)
let () =
    if !help then
        begin
        Arg.usage speclist usage_msg ;
        exit 0
        end

(* Checking if the input file path was provided. *)
let () =
    if !input_path = "" then
    begin
    print_endline "Error: Missing input file path.\n";
    Arg.usage speclist usage_msg;
    exit 1
    end

let instrs =
     match UF.words_of_path !input_path with
        | Error size ->
            print_endline(EP.err (E.InvalidFileSize size));
            exit 1
        | Ok x -> x

let () =
    match D.disassemble_prog instrs with
    | Error e ->
        begin
        print_endline (EP.err e) ;
        exit 2;
        end
    | Ok p ->
        let string_p = IP.prog p in
        let out_path = default_output_path !input_path !output_path in
        let () = UF.write_file out_path string_p in
        print_endline ("Wrote: " ^ out_path)

