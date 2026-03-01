(*
  vm.ml
  Parser for VM programs.
*)

open Utils.Files


let parse string= Vm_parser.prog Vm_lexer.read_token (Lexing.from_string string)

let parse_from_path path = parse (Utils.Files.read_file path)