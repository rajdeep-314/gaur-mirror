(*
  cpu_simulator_web.ml

  Browser interface for the Gaur CPU simulator.
*)

open Js_of_ocaml

module Sast = Syntax.Ast
module Proc = Cpusim.Proc
module Ram = Cpusim.Ram
module Rf = Cpusim.Regfile
module Trace = Cpusim.Trace

module P = Types.Isa.Printers
module RP = Types.Reg.Printers
module IsaParser = Parsing.Isa.Driver
module IsaErrors = Parsing.Isa.Errors

module AsmParser = Parsing.Asm.Driver
module AsmAssemble = Assembler.Assemble
module AsmErrors = Parsing.Asm.Errors
module AsmTypes = Types.Asm

module DU = Dom_utils
module M = Misc

(* State for the CPU simulator web application *)
type app_state = {
  mutable prog : Sast.prog option;
  mutable prog_source : string;
  mutable prog_filename : string;
  mutable initial_ram_words : (int * int) list;
  mutable ram_filename : string;
  mutable debug_proc : Proc.t option;
  mutable debug_steps : int;
  mutable debug_prev_regs : (Sast.reg * int) list;
  mutable debug_traces : string list;
  mutable debug_halted : bool;
  mutable batch_last_trace : string;
}

let state : app_state = {
  prog = None;
  prog_source = "";
  prog_filename = "";
  initial_ram_words = [];
  ram_filename = "";
  debug_proc = None;
  debug_steps = 0;
  debug_prev_regs = [];
  debug_traces = [];
  debug_halted = false;
  batch_last_trace = "";
}

(* Format single trace entry *)
let format_trace_item tr =
  match tr with
  | Trace.RegMod (pc, inst, r, v) ->
      let rval = match r with Sast.R0 -> 0 | _ -> v in
      Printf.sprintf "(%04x)\t%s\n\t%s\t%04x" pc (P.inst inst) (RP.reg r) rval
  | Trace.RamMod (pc, inst, addr, v) ->
      Printf.sprintf "(%04x)\t%s\n\t%04x\t%04x" pc (P.inst inst) addr v
  | Trace.NoTrace (pc, inst) ->
      Printf.sprintf "(%04x)\t%s\n\t-" pc (P.inst inst)

(* Create a clean initial processor state *)
let make_initial_proc () =
  let ram =
    match state.initial_ram_words with
    | [] -> Ram.create ()
    | mmap -> Ram.init_from_mmap mmap
  in
  Proc.State (0, Rf.create (), ram)

(* Read all 32 register values from a regfile *)
let get_all_registers rf =
  List.map (fun reg -> (reg, Rf.read rf reg)) Sast.Helper.reg_list

(* Collect all non-zero entries from RAM *)
let get_all_nonzero_ram ram =
  let rec loop addr acc =
    if addr > Ram.max_addr then List.rev acc
    else
      match Ram.read_safe ram addr with
      | Some v when v <> 0 -> loop (addr + 1) ((addr, v) :: acc)
      | _ -> loop (addr + 1) acc
  in
  loop 0 []

(* Collect a specific range of RAM entries *)
let get_ram_range ram low count =
  let high = min Ram.max_addr (low + count - 1) in
  let rec loop addr acc =
    if addr > high then List.rev acc
    else
      match Ram.read_safe ram addr with
      | Some v -> loop (addr + 1) ((addr, v) :: acc)
      | None -> loop (addr + 1) acc
  in
  loop (max 0 low) []

(* Parse intermediate ISA source *)
let parse_isa_source name source =
  try
    let syn_ast = IsaParser.parse_prog_with_path name source in
    Ok (Sast.T.Isa.remove_annot_from_prog syn_ast)
  with
  | IsaErrors.LexingExn err ->
      Error (IsaErrors.LexingErrors.to_string_with_src err source)
  | IsaErrors.ParsingExn err ->
      Error (IsaErrors.ParsingErrors.to_string_with_src err source)
  | exn ->
      Error ("ISA Parsing error: " ^ Printexc.to_string exn)

(* Parse and assemble Gaur assembly source *)
let parse_and_assemble_gsm name source =
  try
    let raw_ast = AsmParser.parse_prog_with_path name source in
    let stripped = AsmTypes.remove_annot_from_prog raw_ast in
    match AsmAssemble.to_inter stripped with
    | Ok inter_prog -> Ok inter_prog
    | Error e -> Error ("Assembly error: " ^ e)
  with
  | AsmErrors.LexingExn err ->
      Error (AsmErrors.LexingErrors.to_string_with_src err source)
  | AsmErrors.ParsingExn err ->
      Error (AsmErrors.ParsingErrors.to_string_with_src err source)
  | exn ->
      Error ("Assembly parsing error: " ^ Printexc.to_string exn)

(* Load program depending on extension *)
let load_program_from_string name content =
  if Filename.check_suffix name ".gsm" then
    parse_and_assemble_gsm name content
  else
    match parse_isa_source name content with
    | Ok prog -> Ok prog
    | Error e ->
        if Filename.check_suffix name ".inter" then Error e
        else
          (* Fallback: attempt assembly parse if ISA parse failed on ambiguous extension *)
          match parse_and_assemble_gsm name content with
          | Ok prog -> Ok prog
          | Error _ -> Error e

(* Convert int to string decimal *)
let signed_dec_string v =
  string_of_int v

(* Clear children of DOM element *)
let clear_children elem =
  elem##.innerHTML := Js.string ""

(* Render 32 register cards into a container *)
let render_registers_grid container rf prev_regs_opt =
  clear_children container;
  let doc = Dom_html.document in
  let current_regs = get_all_registers rf in

  List.iter (fun (reg, v) ->
    let card = Dom_html.createDiv doc in
    let reg_name_str = RP.reg reg in
    let is_updated =
      match prev_regs_opt with
      | Some prev_regs ->
          (match List.assoc_opt reg prev_regs with
           | Some prev_v -> prev_v <> v
           | None -> false)
      | None -> false
    in
    let card_class =
      if is_updated then "reg-card updated" else "reg-card"
    in
    card##.className := Js.string card_class;

    let header = Dom_html.createDiv doc in
    header##.className := Js.string "reg-card-header";

    let name_span = Dom_html.createSpan doc in
    name_span##.className := Js.string "reg-name";
    name_span##.textContent := Js.some (Js.string reg_name_str);

    let dec_span = Dom_html.createSpan doc in
    dec_span##.className := Js.string "reg-dec";
    dec_span##.textContent := Js.some (Js.string (signed_dec_string v));

    Dom.appendChild header name_span;
    Dom.appendChild header dec_span;

    let hex_div = Dom_html.createDiv doc in
    hex_div##.className := Js.string "reg-hex";
    hex_div##.textContent := Js.some (Js.string (Printf.sprintf "%08x" v));

    Dom.appendChild card header;
    Dom.appendChild card hex_div;
    Dom.appendChild container card;
  ) current_regs

(* Render RAM rows into a tbody *)
let render_ram_table tbody entries =
  clear_children tbody;
  let doc = Dom_html.document in
  List.iter (fun (addr, v) ->
    let tr = Dom_html.createTr doc in

    let td_addr = Dom_html.createTd doc in
    td_addr##.textContent := Js.some (Js.string (Printf.sprintf "%04x" addr));

    let td_hex = Dom_html.createTd doc in
    td_hex##.textContent := Js.some (Js.string (Printf.sprintf "%08x" v));

    let td_dec = Dom_html.createTd doc in
    td_dec##.textContent := Js.some (Js.string (signed_dec_string v));

    Dom.appendChild tr td_addr;
    Dom.appendChild tr td_hex;
    Dom.appendChild tr td_dec;
    Dom.appendChild tbody tr;
  ) entries

(* Render Debugger Program Listing *)
let render_prog_viewer container prog active_pc =
  clear_children container;
  let doc = Dom_html.document in
  List.iteri (fun pc inst ->
    let row = Dom_html.createDiv doc in
    let is_active = (pc = active_pc) in
    row##.className :=
      Js.string (if is_active then "prog-line active-pc" else "prog-line");

    let span_pc = Dom_html.createSpan doc in
    span_pc##.className := Js.string "prog-line-pc";
    span_pc##.textContent := Js.some (Js.string (Printf.sprintf "%04x" pc));

    let span_marker = Dom_html.createSpan doc in
    span_marker##.className := Js.string "prog-line-marker";
    span_marker##.textContent := Js.some (Js.string (if is_active then "→" else " "));

    let span_text = Dom_html.createSpan doc in
    span_text##.className := Js.string "prog-line-text";
    span_text##.textContent := Js.some (Js.string (P.inst inst));

    Dom.appendChild row span_pc;
    Dom.appendChild row span_marker;
    Dom.appendChild row span_text;
    Dom.appendChild container row;

    if is_active then begin
      (* Scroll row into view smoothly *)
      let js_row = Js.Unsafe.coerce row in
      let _ = Js.Unsafe.meth_call js_row "scrollIntoView"
        [| Js.Unsafe.obj [| ("block", Js.Unsafe.inject (Js.string "nearest")) |] |]
      in ()
    end
  ) prog

let () =
  (* Elements lookup *)
  let prog_file_input = DU.get_input "prog-file-input" in
  let prog_file_status = DU.get_element "prog-file-status" in
  let ram_file_input = DU.get_input "ram-file-input" in
  let ram_file_status = DU.get_element "ram-file-status" in
  let clear_ram_btn = DU.get_element "clear-ram-btn" in

  let notification_banner = DU.get_element "notification-banner" in
  let notification_msg = DU.get_element "notification-msg" in
  let notification_close = DU.get_element "notification-close" in

  let tab_batch = DU.get_element "tab-batch" in
  let tab_debug = DU.get_element "tab-debug" in
  let batch_panel = DU.get_element "batch-panel" in
  let debug_panel = DU.get_element "debug-panel" in

  (* Batch controls and outputs *)
  let batch_max_steps = DU.get_input "batch-max-steps" in
  let batch_trace_toggle = DU.get_input "batch-trace-toggle" in
  let batch_fs_toggle = DU.get_input "batch-fs-toggle" in
  let batch_run_btn = DU.get_element "batch-run-btn" in
  let batch_reset_btn = DU.get_element "batch-reset-btn" in

  let batch_results_container = DU.get_element "batch-results-container" in
  let batch_stat_pc = DU.get_element "batch-stat-pc" in
  let batch_stat_steps = DU.get_element "batch-stat-steps" in
  let batch_stat_status = DU.get_element "batch-stat-status" in

  let batch_final_state_section = DU.get_element "batch-final-state-section" in
  let batch_registers_grid = DU.get_element "batch-registers-grid" in
  let batch_ram_tbody = DU.get_element "batch-ram-tbody" in
  let batch_ram_empty = DU.get_element "batch-ram-empty" in

  let batch_trace_section = DU.get_element "batch-trace-section" in
  let batch_trace_output = DU.get_element "batch-trace-output" in
  let batch_copy_trace = DU.get_element "batch-copy-trace" in
  let batch_download_trace = DU.get_element "batch-download-trace" in

  (* Debugger controls and views *)
  let debug_step_btn = DU.get_element "debug-step-btn" in
  let debug_step_n_btn = DU.get_element "debug-step-n-btn" in
  let debug_step_n_count = DU.get_input "debug-step-n-count" in
  let debug_run_btn = DU.get_element "debug-run-btn" in
  let debug_reset_btn = DU.get_element "debug-reset-btn" in

  let debug_pc_val = DU.get_element "debug-pc-val" in
  let debug_step_val = DU.get_element "debug-step-val" in
  let debug_status_val = DU.get_element "debug-status-val" in
  let debug_prog_count = DU.get_element "debug-prog-count" in
  let debug_prog_container = DU.get_element "debug-prog-container" in

  let subtab_regs = DU.get_element "subtab-regs" in
  let subtab_ram = DU.get_element "subtab-ram" in
  let subtab_trace = DU.get_element "subtab-trace" in
  let state_panel_regs = DU.get_element "state-panel-regs" in
  let state_panel_ram = DU.get_element "state-panel-ram" in
  let state_panel_trace = DU.get_element "state-panel-trace" in

  let debug_registers_grid = DU.get_element "debug-registers-grid" in
  let ram_inspect_addr = DU.get_input "ram-inspect-addr" in
  let ram_inspect_count = DU.get_input "ram-inspect-count" in
  let ram_inspect_range_btn = DU.get_element "ram-inspect-range-btn" in
  let ram_inspect_nz_btn = DU.get_element "ram-inspect-nz-btn" in
  let debug_ram_tbody = DU.get_element "debug-ram-tbody" in

  let debug_trace_output = DU.get_element "debug-trace-output" in
  let debug_copy_trace = DU.get_element "debug-copy-trace" in
  let debug_clear_trace = DU.get_element "debug-clear-trace" in

  (* Error elements *)
  let error_section = DU.get_element "error-section" in
  let sim_errors = DU.get_element "sim-errors" in
  let copy_errors = DU.get_element "copy-errors" in

  let show_error msg =
    DU.set_text sim_errors msg;
    DU.show_element error_section
  in

  let clear_error () =
    DU.clear_text sim_errors;
    DU.hide_element error_section
  in

  let show_notification msg =
    DU.set_text notification_msg msg;
    DU.show_element notification_banner
  in

  notification_close##.onclick :=
    Dom_html.handler (fun _ ->
      DU.hide_element notification_banner;
      Js._false
    );

  DU.setup_copy_button copy_errors sim_errors;
  DU.setup_copy_button batch_copy_trace batch_trace_output;
  DU.setup_copy_button debug_copy_trace debug_trace_output;

  let current_batch_download_trace = ref None in
  let current_batch_filename = ref "execution_trace.txt" in
  DU.setup_download_button
    batch_download_trace
    current_batch_download_trace
    current_batch_filename
    sim_errors
    M.download_file;

  (* Tab switching: Batch vs Debug *)
  let set_active_mode mode =
    if mode = "batch" then begin
      tab_batch##.className := Js.string "mode-tab active";
      tab_debug##.className := Js.string "mode-tab";
      batch_panel##.style##.display := Js.string "flex";
      debug_panel##.style##.display := Js.string "none";
    end else begin
      tab_batch##.className := Js.string "mode-tab";
      tab_debug##.className := Js.string "mode-tab active";
      batch_panel##.style##.display := Js.string "none";
      debug_panel##.style##.display := Js.string "flex";
    end
  in

  tab_batch##.onclick :=
    Dom_html.handler (fun _ ->
      set_active_mode "batch";
      Js._false
    );

  tab_debug##.onclick :=
    Dom_html.handler (fun _ ->
      set_active_mode "debug";
      Js._false
    );

  (* Debugger sub-tabs: Regs vs RAM vs Trace *)
  let set_debug_subtab tab =
    subtab_regs##.className := Js.string (if tab = "regs" then "state-tab active" else "state-tab");
    subtab_ram##.className := Js.string (if tab = "ram" then "state-tab active" else "state-tab");
    subtab_trace##.className := Js.string (if tab = "trace" then "state-tab active" else "state-tab");

    state_panel_regs##.style##.display := Js.string (if tab = "regs" then "flex" else "none");
    state_panel_ram##.style##.display := Js.string (if tab = "ram" then "flex" else "none");
    state_panel_trace##.style##.display := Js.string (if tab = "trace" then "flex" else "none");
  in

  subtab_regs##.onclick := Dom_html.handler (fun _ -> set_debug_subtab "regs"; Js._false);
  subtab_ram##.onclick := Dom_html.handler (fun _ -> set_debug_subtab "ram"; Js._false);
  subtab_trace##.onclick := Dom_html.handler (fun _ -> set_debug_subtab "trace"; Js._false);

  (* Sync Debugger UI elements with current state.debug_proc *)
  let sync_debugger_view status_msg =
    match state.debug_proc, state.prog with
    | Some proc, Some prog ->
        let pc = Proc.get_pc proc in
        let rf = Proc.get_regfile proc in
        DU.set_text debug_pc_val (Printf.sprintf "%04x" pc);
        DU.set_text debug_step_val (string_of_int state.debug_steps);
        DU.set_text debug_status_val status_msg;

        (* Render active line in program *)
        render_prog_viewer debug_prog_container prog pc;

        (* Render registers with change diffs *)
        render_registers_grid debug_registers_grid rf (Some state.debug_prev_regs);

        (* Update trace box *)
        DU.set_text debug_trace_output (String.concat "\n" (List.rev state.debug_traces))

    | None, _ ->
        DU.set_text debug_pc_val "0000";
        DU.set_text debug_step_val "0";
        DU.set_text debug_status_val "No state";
        clear_children debug_registers_grid;
        clear_children debug_prog_container

    | _, None ->
        DU.set_text debug_pc_val "0000";
        DU.set_text debug_step_val "0";
        DU.set_text debug_status_val "No program";
        clear_children debug_registers_grid;
        clear_children debug_prog_container
  in

  (* Reset debugger state *)
  let reset_debugger () =
    state.debug_proc <- Some (make_initial_proc ());
    state.debug_steps <- 0;
    state.debug_prev_regs <- [];
    state.debug_traces <- [];
    state.debug_halted <- false;
    sync_debugger_view "Ready"
  in

  debug_reset_btn##.onclick :=
    Dom_html.handler (fun _ ->
      reset_debugger ();
      Js._false
    );

  debug_clear_trace##.onclick :=
    Dom_html.handler (fun _ ->
      state.debug_traces <- [];
      DU.clear_text debug_trace_output;
      Js._false
    );

  (* Debugger Single Step *)
  let execute_single_step () =
    if state.debug_halted then ()
    else
      match state.prog, state.debug_proc with
      | None, _ ->
          show_error "Please load a program first."
      | _, None ->
          show_error "Simulator state is not initialized."
      | Some prog, Some proc ->
          clear_error ();
          let current_pc = Proc.get_pc proc in
          match List.nth_opt prog current_pc with
          | None ->
              state.debug_halted <- true;
              sync_debugger_view "Halted (End of program)"
          | Some _ ->
              let current_rf = Proc.get_regfile proc in
              state.debug_prev_regs <- get_all_registers current_rf;
              match Proc.step_prog_wt proc prog with
              | None ->
                  state.debug_halted <- true;
                  sync_debugger_view "Halted (End of program)"
              | Some (proc', tr) ->
                  let next_pc = Proc.get_pc proc' in
                  state.debug_proc <- Some proc';
                  state.debug_steps <- state.debug_steps + 1;
                  state.debug_traces <- format_trace_item tr :: state.debug_traces;
                  if next_pc = current_pc then begin
                    state.debug_halted <- true;
                    sync_debugger_view "Halted"
                  end else begin
                    sync_debugger_view "Running"
                  end
  in

  debug_step_btn##.onclick :=
    Dom_html.handler (fun _ ->
      execute_single_step ();
      Js._false
    );

  (* Debugger Step N *)
  let execute_step_n count =
    if state.debug_halted then ()
    else
      match state.prog, state.debug_proc with
      | None, _ ->
          show_error "Please load a program first."
      | _, None ->
          show_error "Simulator state is not initialized."
      | Some prog, Some _ ->
          clear_error ();
          let rec loop remaining =
            if remaining <= 0 then "Running"
            else
              match state.debug_proc with
              | None -> "No state"
              | Some proc ->
                  let current_pc = Proc.get_pc proc in
                  match List.nth_opt prog current_pc with
                  | None ->
                      state.debug_halted <- true;
                      "Halted (End of program)"
                  | Some _ ->
                      let current_rf = Proc.get_regfile proc in
                      state.debug_prev_regs <- get_all_registers current_rf;
                      match Proc.step_prog_wt proc prog with
                      | None ->
                          state.debug_halted <- true;
                          "Halted (End of program)"
                      | Some (proc', tr) ->
                          let next_pc = Proc.get_pc proc' in
                          state.debug_proc <- Some proc';
                          state.debug_steps <- state.debug_steps + 1;
                          state.debug_traces <- format_trace_item tr :: state.debug_traces;
                          if next_pc = current_pc then begin
                            state.debug_halted <- true;
                            "Halted"
                          end else begin
                            loop (remaining - 1)
                          end
          in
          let status = loop count in
          sync_debugger_view status
  in

  debug_step_n_btn##.onclick :=
    Dom_html.handler (fun _ ->
      let count =
        try int_of_string (Js.to_string debug_step_n_count##.value)
        with _ -> 10
      in
      execute_step_n (max 1 count);
      Js._false
    );

  (* Debugger Run to End *)
  debug_run_btn##.onclick :=
    Dom_html.handler (fun _ ->
      if not state.debug_halted then
        execute_step_n 100000;
      Js._false
    );

  (* RAM Inspector buttons *)
  ram_inspect_range_btn##.onclick :=
    Dom_html.handler (fun _ ->
      match state.debug_proc with
      | None ->
          show_error "Simulator state is not initialized.";
          Js._false
      | Some proc ->
          let addr =
            try int_of_string (Js.to_string ram_inspect_addr##.value)
            with _ -> 0
          in
          let count =
            try int_of_string (Js.to_string ram_inspect_count##.value)
            with _ -> 16
          in
          let entries = get_ram_range (Proc.get_ram proc) addr count in
          render_ram_table debug_ram_tbody entries;
          Js._false
    );

  ram_inspect_nz_btn##.onclick :=
    Dom_html.handler (fun _ ->
      match state.debug_proc with
      | None ->
          show_error "Simulator state is not initialized.";
          Js._false
      | Some proc ->
          let entries = get_all_nonzero_ram (Proc.get_ram proc) in
          render_ram_table debug_ram_tbody entries;
          Js._false
    );

  (* Successful program load setup *)
  let on_program_loaded filename source prog =
    state.prog <- Some prog;
    state.prog_source <- source;
    state.prog_filename <- filename;
    prog_file_status##.className := Js.string "file-status loaded";
    DU.set_text prog_file_status (Printf.sprintf "Loaded: %s (%d instrs)" filename (List.length prog));
    DU.set_text debug_prog_count (Printf.sprintf "%d instructions" (List.length prog));
    reset_debugger ();
    clear_error ()
  in

  (* Setup Program File Input *)
  prog_file_input##.onchange :=
    Dom_html.handler (fun _ ->
      clear_error ();
      match Js.Opt.to_option prog_file_input##.files with
      | None -> Js._false
      | Some files ->
          if files##.length > 0 then
            match Js.Opt.to_option (files##item 0) with
            | None -> Js._false
            | Some file ->
                let filename = Js.to_string file##.name in
                DU.read_file_as_text file (function
                  | None -> show_error ("Failed to read file: " ^ filename)
                  | Some content ->
                      match load_program_from_string filename content with
                      | Error err -> show_error err
                      | Ok prog -> on_program_loaded filename content prog
                );
                Js._false
          else Js._false
    );

  (* Setup RAM File Input *)
  ram_file_input##.onchange :=
    Dom_html.handler (fun _ ->
      clear_error ();
      match Js.Opt.to_option ram_file_input##.files with
      | None -> Js._false
      | Some files ->
          if files##.length > 0 then
            match Js.Opt.to_option (files##item 0) with
            | None -> Js._false
            | Some file ->
                let filename = Js.to_string file##.name in
                let reader = new%js File.fileReader in
                reader##.onload :=
                  Dom.handler (fun _ ->
                    match Js.Opt.to_option (File.CoerceTo.arrayBuffer reader##.result) with
                    | None ->
                        show_error "Could not read RAM file as ArrayBuffer.";
                        Js._false
                    | Some buffer ->
                        match M.words_of_array_buffer buffer with
                        | Error size ->
                            show_error (Printf.sprintf "Initial RAM image size (%d bytes) is not a multiple of 4." size);
                            Js._false
                        | Ok words ->
                            state.initial_ram_words <- List.mapi (fun i v -> (i, v)) words;
                            state.ram_filename <- filename;
                            ram_file_status##.className := Js.string "file-status loaded";
                            DU.set_text ram_file_status (Printf.sprintf "Loaded: %s (%d words)" filename (List.length words));
                            reset_debugger ();
                            Js._false
                  );
                reader##readAsArrayBuffer file;
                Js._false
          else Js._false
    );

  (* Clear RAM button *)
  clear_ram_btn##.onclick :=
    Dom_html.handler (fun _ ->
      state.initial_ram_words <- [];
      state.ram_filename <- "";
      ram_file_input##.value := Js.string "";
      ram_file_status##.className := Js.string "file-status";
      DU.set_text ram_file_status "Zero-initialized RAM";
      reset_debugger ();
      Js._false
    );

  (* Batch Run Execution *)
  batch_run_btn##.onclick :=
    Dom_html.handler (fun _ ->
      clear_error ();
      match state.prog with
      | None ->
          show_error "Please upload an intermediate program (.gsm.inter) or Gaur assembly (.gsm) first.";
          Js._false
      | Some prog ->
          let max_steps =
            try int_of_string (Js.to_string batch_max_steps##.value)
            with _ -> 2000
          in
          let trace_on = Js.to_bool batch_trace_toggle##.checked in
          let final_state_on = Js.to_bool batch_fs_toggle##.checked in

          let init_proc = make_initial_proc () in

          (* Execute batch with traces and halt detection *)
          let rec run_loop proc traces count =
            if count >= max_steps then
              (proc, traces, count, "Max steps reached")
            else
              let pc = Proc.get_pc proc in
              match List.nth_opt prog pc with
              | None ->
                  (proc, traces, count, "Halted")
              | Some _ ->
                  match Proc.step_prog_wt proc prog with
                  | None ->
                      (proc, traces, count, "Halted")
                  | Some (proc', tr) ->
                      let next_pc = Proc.get_pc proc' in
                      let traces' =
                        if trace_on then (format_trace_item tr) :: traces
                        else traces
                      in
                      if next_pc = pc then
                        (proc', traces', count + 1, "Halted")
                      else
                        run_loop proc' traces' (count + 1)
          in

          let final_proc, rev_traces, total_steps, status =
            run_loop init_proc [] 0
          in

          let final_pc = Proc.get_pc final_proc in
          let final_rf = Proc.get_regfile final_proc in
          let final_ram = Proc.get_ram final_proc in

          (* Update summary badges *)
          DU.set_text batch_stat_pc (Printf.sprintf "%04x" final_pc);
          DU.set_text batch_stat_steps (string_of_int total_steps);
          DU.set_text batch_stat_status status;

          (* Render Final State *)
          if final_state_on then begin
            render_registers_grid batch_registers_grid final_rf None;
            let nz_ram = get_all_nonzero_ram final_ram in
            if nz_ram = [] then begin
              DU.show_element batch_ram_empty;
              clear_children batch_ram_tbody;
            end else begin
              DU.hide_element batch_ram_empty;
              render_ram_table batch_ram_tbody nz_ram;
            end;
            DU.show_element batch_final_state_section;
          end else begin
            DU.hide_element batch_final_state_section;
          end;

          (* Render Trace *)
          if trace_on then begin
            let trace_text = String.concat "\n" (List.rev rev_traces) in
            state.batch_last_trace <- trace_text;
            current_batch_download_trace := Some trace_text;
            DU.set_text batch_trace_output trace_text;
            DU.show_element batch_trace_section;
          end else begin
            state.batch_last_trace <- "";
            current_batch_download_trace := None;
            DU.clear_text batch_trace_output;
            DU.hide_element batch_trace_section;
          end;

          DU.show_element batch_results_container;
          Js._false
    );

  (* Batch Reset button *)
  let reset_batch () =
    DU.hide_element batch_results_container;
    DU.set_text batch_stat_pc "0000";
    DU.set_text batch_stat_steps "0";
    DU.set_text batch_stat_status "Ready";
    clear_children batch_registers_grid;
    clear_children batch_ram_tbody;
    DU.clear_text batch_trace_output;
    DU.hide_element batch_trace_section;
    clear_error ()
  in

  batch_reset_btn##.onclick :=
    Dom_html.handler (fun _ ->
      reset_batch ();
      Js._false
    );

  (* Check sessionStorage for program transferred from Assembler *)
  let check_session_storage () =
    try
      let ss_opt = Js.Optdef.to_option Dom_html.window##.sessionStorage in
      match ss_opt with
      | None -> ()
      | Some ss ->
          let opt_prog = ss##getItem (Js.string "gaur_sim_program") in
          Js.Opt.iter opt_prog (fun js_str ->
            let prog_str = Js.to_string js_str in
            if String.trim prog_str <> "" then begin
              ss##removeItem (Js.string "gaur_sim_program");
              let filename =
                match Js.Opt.to_option (ss##getItem (Js.string "gaur_sim_filename")) with
                | Some js_name -> Js.to_string js_name
                | None -> "from_assembler.gsm.inter"
              in
              ss##removeItem (Js.string "gaur_sim_filename");
              match load_program_from_string filename prog_str with
              | Ok prog ->
                  DU.set_input_file prog_file_input filename prog_str;
                  on_program_loaded filename prog_str prog;
                  show_notification ("Loaded assembled intermediate program (" ^ filename ^ ") from Assembler.")
              | Error err ->
                  show_error ("Failed to load program from Assembler: " ^ err)
            end
          )
    with _ -> ()
  in

  check_session_storage ()
