open Hdlutils

module type CHIP = sig
  type state
  type input
  type output

  val default_input : input
  val default_state : state

  val output : input -> state -> output
  val clock : input -> state -> state

  val typ_of_input : Parse.Types.typ
  val input_of_expr : 'a Parse.Ast.expr -> input
  val expr_of_output : output -> 'a Parse.Ast.expr
end

let repl outx (module C : CHIP) =
  let state = ref C.default_state in
  let input = ref C.default_input in
  let rec go () =
    let cmd = Prompt.read outx () in
    match cmd with
    | Input e ->
      let open Typecheck in
      (try Typechecker.tc_input C.typ_of_input e with
       | Errors.Error (err, loc) ->
         Io.output_stringln outx (Fmt.to_to_string Errors.Pretty.pp (err, loc)));
      input := C.input_of_expr e.expr;
      go ()

    | Output ->
      let output = C.output !input !state in
      let output_expr = C.expr_of_output output in
      let output_str = Fmt.to_to_string Parse.Ast.Pretty.pp_expr output_expr in
      Io.output_stringln outx output_str;
      go ()

    | Clock ->
      state := C.clock !input !state;
      go ()

    | Quit -> ()
  in
  go ()
;;
