open PHDL
open PHDL.ParseHelper

(** [parse s] parses [s] into an AST *)
let _parse (s : string) : Ast.program = 
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.read lexbuf in
  ast

let () =
  let filename = Sys.argv.(1) in
  let text = attempt1 filename in
  attempt2 filename text
