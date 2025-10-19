open PHDL
open PHDL.ParseHelper

(** [parse s] parses [s] into an AST *)
let _parse (s : string) : Ast.program = 
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.read lexbuf in
  ast

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    let filename = Sys.argv.(i) in
    let result = attempt1 filename in
    match result with
    | Error text -> ignore (attempt2 filename text)
    | Ok program -> 
      let env = Init_env.init_chip program in
      ignore (Typecheck.tc_program env program)
  done
