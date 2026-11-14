(** High-level, I/O-free API for compiling and simulating GHDL programs. *)

open Common

module CmdParse     = Cmd.Parse
module CmdTypecheck = Cmd.Typecheck

type compiled = Ast.prog

let compile (sources : Source.t list) : (compiled, string) result =
  let resolver = Source.of_list sources in
  try
    let parsed = Parse.parse_sources sources in
    match Analyze.Toposort.toposort_result parsed with
    | Error msg -> Error msg
    | Ok sorted ->
      match Typecheck.typecheck_result ~resolver sorted with
      | Error msg -> Error msg
      | Ok typed ->
        match Analyze.Causality.analyse_result ~resolver typed with
        | Error msg -> Error msg
        | Ok causal ->
          let analysed = Analyze.Liveness.analyse causal in
          Ok (Reduce.reduce_prog analysed)
  with
  | Parse.Errors.Parse_error err ->
    Error (Fmt.to_to_string (Parse.Errors.pp_parse_error ~resolver) err)

let compile_string ?(name = "<string>") content : (compiled, string) result =
  compile [ Source.from_string ~name content ]

let chip_names (prog : compiled) : string list =
  let names = List.map (fun (c : Ast.chip) -> c.name) prog in
  if List.mem Interp.Builtin.Nand.name names then names
  else Interp.Builtin.Nand.name :: names

let nand_count (prog : compiled) (name : string) : int =
  Ast.StrMap.find_opt name (Interp.nand_counts prog)
  |> Option.value ~default:0

type build_phase = Repl.build_phase =
  | Building of { built : int; total : int }
  | Stabilizing of { total : int }

type session =
  { chip_name : string
  ; prog      : compiled
  ; mutable sim  : Repl.sim
  ; mutable time : int
  }

let format_int_commas n =
  let s = string_of_int n in
  let len = String.length s in
  let buf = Buffer.create (len + 8) in
  let prefix = len mod 3 in
  let start = if prefix = 0 then 3 else prefix in
  Buffer.add_substring buf s 0 start;
  let pos = ref start in
  while !pos < len do
    Buffer.add_char buf ',';
    Buffer.add_substring buf s !pos 3;
    pos := !pos + 3
  done;
  Buffer.contents buf

let create ?on_progress ?max_nands ~chip_name (prog : compiled) : (session, string) result =
  let count = nand_count prog chip_name in
  match max_nands with
  | Some limit when count > limit ->
    Error
      (Printf.sprintf
         "Chip %s requires %s Nand gates, which exceeds the browser simulation limit of %s Nand gates. (Simulate large chips using the native gaur CLI)."
         chip_name (format_int_commas count) (format_int_commas limit))
  | _ ->
    match Repl.build_result ?on_progress chip_name prog with
    | Error msg -> Error msg
    | Ok sim -> Ok { chip_name; prog; sim; time = 0 }

let reset (s : session) : (unit, string) result =
  Repl.reset s.sim;
  s.time <- 0;
  Ok ()

let time (s : session) : int = s.time
let chip_name (s : session) : string = s.chip_name

let input_type_string (s : session) : string =
  Fmt.to_to_string CmdParse.Types.Pretty.pp_t s.sim.input_t

let input_type (s : session) : CmdParse.Types.typ =
  s.sim.input_t

let set_input (s : session) (v : bool Value.shape) : (unit, string) result =
  try
    Repl.set_input s.sim v;
    Ok ()
  with Value.Shape_mismatch msg ->
    Error msg

let output_shape (s : session) : bool Value.shape =
  Repl.output s.sim

type cmd_output =
  | Input_set
  | Output of { time : int; value : string }
  | Clocked of { time : int }
  | Quit

let parse_cmd line =
  try Ok (CmdParse.parse "<web-cmd>" line)
  with
  | CmdParse.Errors.Error err ->
    Error (Fmt.to_to_string CmdParse.Errors.Pretty.pp err)
  | CmdParse.Parser.Error st ->
    Error (CmdParse.Messages.message st)

let typecheck_input (sim : Repl.sim) e =
  try Ok (CmdTypecheck.Typechecker.tc_input sim.input_t e)
  with CmdTypecheck.Errors.Error (err, loc) ->
    Error (Fmt.to_to_string CmdTypecheck.Errors.Pretty.pp (err, loc))

let eval_cmd (s : session) (line : string) : (cmd_output, string) result =
  match parse_cmd (String.trim line) with
  | Error msg -> Error msg
  | Ok Quit -> Ok Quit
  | Ok (Input e) ->
    (match typecheck_input s.sim e with
     | Error msg -> Error msg
     | Ok () ->
       Repl.set_input s.sim (Repl.shape_of_expr e);
       Ok Input_set)
  | Ok Output ->
    let e = Repl.expr_of_shape (Repl.output s.sim) in
    let value = Fmt.to_to_string CmdParse.Ast.Pretty.pp_expr e.expr in
    Ok (Output { time = s.time; value })
  | Ok Clock ->
    Repl.clock s.sim;
    s.time <- s.time + 1;
    Ok (Clocked { time = s.time })

let eval_script (s : session) (script : string) : (cmd_output list, string) result =
  let lines =
    script
    |> String.split_on_char '\n'
    |> List.map String.trim
    |> List.filter (fun l -> l <> "")
  in
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | line :: rest ->
      match eval_cmd s line with
      | Error msg -> Error msg
      | Ok Quit -> Ok (List.rev (Quit :: acc))
      | Ok res -> go (res :: acc) rest
  in
  go [] lines
