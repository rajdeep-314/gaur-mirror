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

let time = ref 0
let string = "> "
let show_prompt outx =
  output_string outx string;
  Out_channel.flush outx


let repl inx outx (module C : CHIP) fname prompt exiterr =
  let state = ref C.default_state in
  let input = ref C.default_input in
  let rec go () =
    if prompt then show_prompt outx;
    let line = Io.read_line inx in
    match String.trim line with
    | "" -> go ()
    | s ->
      (match
         (try Ok (Parse.parse fname s)
          with
          | Parse.Errors.Error err ->
            Io.output_stringln outx (Fmt.to_to_string Parse.Errors.Pretty.pp err);
            Error ()
          | Parse.Parser.Error st ->
            Io.output_stringln outx (Parse.Messages.message st);
            Error ())
       with
       | Error () ->
         if exiterr then exit 1;
         go ()
       | Ok cmd ->
         match cmd with
         | Input e ->
           (match
              (try Ok (Typecheck.Typechecker.tc_input C.typ_of_input e)
               with
               | Typecheck.Errors.Error (err, loc) ->
                 Io.output_stringln outx (Fmt.to_to_string Typecheck.Errors.Pretty.pp (err, loc));
                 Error ())
            with
            | Error () ->
              if exiterr then exit 1;
              go ()
            | Ok () ->
              input := C.input_of_expr e.expr;
              go ())

         | Output ->
           let out = C.output !input !state in
           let expr = C.expr_of_output out in
           let str = Fmt.to_to_string Parse.Ast.Pretty.pp_expr expr in
           output_string outx (Format.sprintf "t: %d\t | " !time);
           Io.output_stringln outx str;
           go ()

         | Clock ->
           state := C.clock !input !state;
           incr time;
           go ()

         | Quit -> ())
  in
  go ()
