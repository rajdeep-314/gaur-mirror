(** Web Worker running GHDL compilation and simulation off the main UI thread. *)

open Js_of_ocaml
open Hdl_simulator

module Api      = Backend.Interpreter.Api
module Value    = Backend.Interpreter.Value
module CmdTypes = Backend.Interpreter.Cmd.Parse.Types
module P        = Hdl_sim_proto

(** Converts an internal type into a serializable wire type for UI generation *)
let rec wire_of_typ (t : CmdTypes.typ) : P.wire_typ =
  match t with
  | CmdTypes.TBits n -> P.WTBits n
  | CmdTypes.TRecord m ->
    P.WTRecord (CmdTypes.StrMap.bindings m |> List.map (fun (k, v) -> (k, wire_of_typ v)))

(** Converts an internal boolean shape into a serializable wire shape *)
let rec wire_of_shape (s : bool Value.shape) : P.wire_shape =
  match s with
  | Value.Bitvec arr -> P.WSBitvec arr
  | Value.Record m ->
    P.WSRecord (CmdTypes.StrMap.bindings m |> List.map (fun (k, v) -> (k, wire_of_shape v)))

(** Converts a serializable wire shape back into an internal boolean shape *)
let rec shape_of_wire (ws : P.wire_shape) : bool Value.shape =
  match ws with
  | P.WSBitvec arr -> Value.Bitvec arr
  | P.WSRecord fields ->
    let m =
      List.fold_left
        (fun acc (k, v) -> CmdTypes.StrMap.add k (shape_of_wire v) acc)
        CmdTypes.StrMap.empty
        fields
    in
    Value.Record m

(** Formats runtime and JS exceptions into friendly error messages *)
let friendly_exn exn =
  let raw =
    match exn with
    | Stack_overflow ->
      "Chip is too large to simulate in the browser (stack overflow)."
    | Out_of_memory ->
      "Chip is too large to simulate in the browser (out of memory)."
    | Js_error.Exn js_err ->
      let msg = Js_error.message js_err in
      let lower = String.lowercase_ascii msg in
      if String.length lower >= 5 &&
         (try ignore (Str.search_forward (Str.regexp_string "stack") lower 0); true with Not_found -> false)
         || (try ignore (Str.search_forward (Str.regexp_string "recursion") lower 0); true with Not_found -> false)
      then
        "Chip is too large to simulate in the browser (JavaScript call stack overflow)."
      else
        "Browser error: " ^ msg
    | other ->
      Printexc.to_string other
  in
  raw

(** Active compiled program and active simulation session *)
let compiled_ref : Api.compiled option ref = ref None
let session_ref  : Api.session option ref  = ref None

(** Posts an encoded response message back to the main UI thread *)
let send (r : P.resp) =
  Worker.post_message (P.encode r)

(** Instantiates a chip instance with gate limits and progress notifications *)
let handle_load chip_name =
  match !compiled_ref with
  | None ->
    send (P.Resp_error { msg = "No compiled program."; clear_session = true })
  | Some prog ->
    let on_progress = function
      | Api.Building { built; total } ->
        let pct = if total > 0 then (built * 100) / total else 100 in
        send (P.Resp_progress
                { msg = Printf.sprintf "Building %s: %d%% (%d / %d Nands)..."
                          chip_name pct built total
                ; pct = Some pct
                })
      | Api.Stabilizing { total } ->
        send (P.Resp_progress
                { msg = Printf.sprintf "Stabilizing %s (%d Nands)..."
                          chip_name total
                ; pct = None
                })
    in
    match Api.create ~max_nands:2_000_000 ~on_progress ~chip_name prog with
    | Error msg ->
      send (P.Resp_error { msg; clear_session = false })
    | Ok s ->
      session_ref := Some s;
      send (P.Resp_loaded
              { chip = Api.chip_name s
              ; nands = Api.nand_count prog chip_name
              ; time = Api.time s
              ; input_type_str = Api.input_type_string s
              ; input_type = wire_of_typ (Api.input_type s)
              })

(** Dispatches incoming requests from the UI thread *)
let handle_req (req : P.req) =
  try
    match req with
    | P.Req_compile content ->
      (match Api.compile_string ~name:"<web-input>" content with
       | Error msg ->
         compiled_ref := None;
         session_ref := None;
         send (P.Resp_error { msg; clear_session = true })
       | Ok prog ->
         compiled_ref := Some prog;
         session_ref := None;
         let names = Api.chip_names prog in
         let selected =
           match List.rev names with
           | top :: _ -> Some top
           | [] -> None
         in
         send (P.Resp_compiled { names; selected }))
    | P.Req_load chip_name ->
      handle_load chip_name
    | P.Req_set_input ws ->
      (match !session_ref with
       | None -> ()
       | Some s ->
         match Api.set_input s (shape_of_wire ws) with
         | Ok () -> send P.Resp_input_set
         | Error msg -> send (P.Resp_error { msg; clear_session = false }))
    | P.Req_output ->
      (match !session_ref with
       | None ->
         send (P.Resp_error { msg = "No chip loaded. Compile GHDL source first."; clear_session = false })
       | Some s ->
         let shape = Api.output_shape s in
         send (P.Resp_output { time = Api.time s; shape = wire_of_shape shape }))
    | P.Req_clock ->
      (match !session_ref with
       | None ->
         send (P.Resp_error { msg = "No chip loaded. Compile GHDL source first."; clear_session = false })
       | Some s ->
         match Api.eval_cmd s "CLOCK" with
         | Ok (Api.Clocked { time }) -> send (P.Resp_clocked { time })
         | Ok _ -> ()
         | Error msg -> send (P.Resp_error { msg; clear_session = false }))
    | P.Req_reset ->
      (match !session_ref with
       | None ->
         send (P.Resp_error { msg = "No chip loaded."; clear_session = false })
       | Some s ->
         match Api.reset s with
         | Ok () -> send (P.Resp_reset { time = Api.time s })
         | Error msg -> send (P.Resp_error { msg; clear_session = false }))
  with exn ->
    send (P.Resp_error { msg = friendly_exn exn; clear_session = false })

(** Worker message handler registration *)
let () =
  Worker.set_onmessage (fun msg ->
    let req : P.req = P.decode msg in
    handle_req req)

