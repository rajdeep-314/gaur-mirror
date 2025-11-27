(*
    main.ml

    Make a basic top-level here.
    Just take input file name as cmd line input and an optional output file name.
*)


(*
Parameters:
    - Output file, by default the file name without an extension (remove the last
      dot and the characters after that)
    - Output format (ascii or inter or bin, default bin)
        - ascii: each line is a 16-character ASCII string with 0s and 1s
        - inter: the assembly, with labels resolved
        - bin:   the binary format

Example invocations:
    as foo.pasm
        => makes foo.pmac - the assembled binary
    as bar.pasm --format=inter --out=meow
        => makes meow - the intermediate assembly
    as baz.meow.purr --format=ascii
        => makes baz.meow.pasc - the ASCII representation
*)


module Types = Assembler.Types
module Assemble = Assembler.Assemble
module Parser   =  Assembler.Parser
module Printers = Assembler.Printers



(* Parameters*)
let path = ref ""
let format = ref "bin"
let out = ref ""

let speclist = [
    ("--format",Arg.Set_string format,"Set format of output (bin,inter,ascii)");
    ("--out",Arg.Set_string out,"Setting Output file")
]

let usage_msg = "Usage: as [options] <file_path>."

let remove_extension filename =
  match String.rindex_opt filename '.' with
  | None -> filename
  | Some idx -> String.sub filename 0 idx

let default_output_name str =
  match !format with
  | "bin" -> remove_extension str ^ ".pmac"
  | "ascii" -> remove_extension str ^ ".pasc"
  | "inter" -> remove_extension str ^ ".pasm.inter"
  | _ -> failwith "unknown format"


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
     if !path = "" then (
    print_endline "Error: missing input file.";
    Arg.usage speclist usage_msg;
    exit 1
  )

let outfile =
  if !out <> "" then default_output_name !out else default_output_name !path

let prog =
    Parser.parse_prog_from_path !path

let inter : (Types.isa_prog, string) Result.t = Assemble.to_inter prog
let mp : (Types.machine_prog, string) Result.t = Assemble.assemble_prog prog

let output_str = 
    match mp with 
    | Error e -> print_endline ("Error: " ^ e);
                exit 1
    | Ok mp -> match !format with
        | "ascii" -> Printers.machine_prog_to_ascii mp
        | "bin"   -> Printers.machine_prog_to_bin mp
        | "inter" ->
                begin
                match inter with
                | Error e   -> print_endline ("Error: " ^ e); exit 1
                | Ok inter  -> Printers.isa_prog_to_string inter
                end
        | _ ->
            print_endline ("Unknown format: " ^ !format);
            exit 1
    
let () =
  let oc = open_out outfile in
  output_string oc output_str;
  close_out oc;
  Printf.printf "Wrote: %s\n" outfile
