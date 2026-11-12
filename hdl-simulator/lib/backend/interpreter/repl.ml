(** Read-eval-print loop simulating one chip with the interpreter.

    The incremental DAG of the chip is built exactly once:
    + [vars]    : unwatched [bool Var.t] leaves for the input and the state;
    + [watched] : the same leaves watched, i.e. [bool Incr.t] nodes;
    + [out]     : the compiled chip function applied to [watched];
    + [obs]     : observers on the output and on the next state.

    Commands:
    - [INPUT] sets the input variables
    - [OUTPUT] stabilizes and prints the output,
    - [CLOCK] stabilizes and copies the next state into the state variables.
*)

open Value

module StrMap = Ast.StrMap
module CmdParse = Cmd.Parse
module CmdTypecheck = Cmd.Typecheck

let prompt_string = "> "

let show_prompt outx =
  output_string outx prompt_string;
  Out_channel.flush outx

(** {1 Conversions between runtime expressions and values} *)

let rec typ_of_tipe (t : Ast.tipe) : CmdParse.Types.typ =
  match t with
  | TBitvec n -> TBits n
  | TRecord r -> TRecord (StrMap.map typ_of_tipe r)

let rec shape_of_expr (ei : 'a CmdParse.Ast.expr_info) : bool shape =
  match ei.expr with
  | Const bv -> Bitvec bv
  | Record r -> Record (StrMap.map shape_of_expr r)

let rec expr_of_shape (v : bool shape) : unit CmdParse.Ast.expr_info =
  let expr : unit CmdParse.Ast.expr =
    match v with
    | Bitvec bv -> Const bv
    | Record r  -> Record (StrMap.map expr_of_shape r)
  in
  CmdParse.Ast.mk_expr_info expr ()

(** {1 Simulation} *)

(** A chip instance under simulation. *)
type sim =
  { input_t : CmdParse.Types.typ
  ; vars    : bool Var.t input_interface
  ; obs     : bool Incr.Observer.t output_interface
  }

let fail_build name msg =
  prerr_endline (Printf.sprintf "Error: can not simulate chip %s: %s" name msg);
  exit 1

let build (name : string) (p : Ast.prog) : sim =
  try
    let env = Interp.interp_prog p in
    let def =
      match Ast.StrMap.find_opt name env with
      | Some def -> def
      | None -> fail_build name "no chip with this name"
    in
    let vars = create_input_interface def.input_t def.state_t (fun () -> Var.create false) in
    let watched = map_input_interface Var.watch vars in
    let out = def.fn watched in
    let obs = map_output_interface Incr.observe out in
    Incr.stabilize ();
    { input_t=typ_of_tipe def.input_t; vars; obs }
  with
  | Interp.Interp_error msg | Shape_mismatch msg | Invalid_argument msg ->
    fail_build name msg

let set_input (s : sim) (v : bool shape) =
  iter2 Var.set s.vars.input v

let output (s : sim) : bool shape =
  Incr.stabilize ();
  map Incr.Observer.value_exn s.obs.output

let clock (s : sim) =
  (* Stabilize first, so that the next state reflects the latest input.
     All values are read before any variable is set.
  *)
  Incr.stabilize ();
  let next = map_state Incr.Observer.value_exn s.obs.state in
  iter2_state Var.set s.vars.state next

(** {1 Loop} *)

let parse_cmd outx s =
  try Ok (CmdParse.parse "stdin" s)
  with
  | CmdParse.Errors.Error err ->
    Io.output_stringln outx (Fmt.to_to_string CmdParse.Errors.Pretty.pp err);
    Error ()
  | CmdParse.Parser.Error st ->
    Io.output_stringln outx (CmdParse.Messages.message st);
    Error ()

let typecheck_input outx (s : sim) e =
  try Ok (CmdTypecheck.Typechecker.tc_input s.input_t e)
  with CmdTypecheck.Errors.Error (err, loc) ->
    Io.output_stringln outx (Fmt.to_to_string CmdTypecheck.Errors.Pretty.pp (err, loc));
    Error ()

(** [repl name p] simulates the chip [name] of program [p] reading commands
    from standard input. If [prompt], a prompt is shown before every command.
    If [exiterr], the process exits on the first erroneous command.
*)
let repl ?(prompt = true) ?(exiterr = false) (name : string) (p : Ast.prog) =
  let inx = stdin in
  let outx = stdout in
  let sim = build name p in
  let time = ref 0 in
  let on_error () = if exiterr then exit 1 in

  let rec go () =
    if prompt then show_prompt outx;
    match String.trim (Io.read_line inx) with
    | "" -> go ()
    | s ->
      match parse_cmd outx s with
      | Error () -> on_error (); go ()
      | Ok Quit -> ()
      | Ok (Input e) ->
        (match typecheck_input outx sim e with
         | Error () -> on_error ()
         | Ok () -> set_input sim (shape_of_expr e));
        go ()
      | Ok Output ->
        let e = expr_of_shape (output sim) in
        let str = Fmt.to_to_string CmdParse.Ast.Pretty.pp_expr e.expr in
        output_string outx (Printf.sprintf "t: %d\t | " !time);
        Io.output_stringln outx str;
        go ()
      | Ok Clock ->
        clock sim;
        incr time;
        go ()
  in
  go ()
