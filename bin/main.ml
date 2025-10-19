open PHDL
open PHDL.ParseHelper

(** [parse s] parses [s] into an AST *)
let _parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.read lexbuf in
  ast

let bind_error r f =
  match r with
  | Ok a -> Ok a
  | Error e -> f e

let helper filename =
  bind_error (attempt1 filename) (attempt2 filename)

let accu p f =
  let fp = helper f in
  match fp with
  | Ok a -> a @ p
  | Error e -> e

let foo =
  let files = Array.to_list Sys.argv in
  List.fold_left accu [] (List.tl files)

let () =
  let env = Init_env.init_chip foo in
  ignore (Typecheck.tc_program env foo)


(* let () =
   for i = 1 to Array.length Sys.argv - 1 do
    let filename = Sys.argv.(i) in
    let result = attempt1 filename in
    match result with
    | Error text -> ignore (attempt2 filename text)
    | Ok program ->
      let env = Init_env.init_chip program in
      ignore (Typecheck.tc_program env program)
   done *)
