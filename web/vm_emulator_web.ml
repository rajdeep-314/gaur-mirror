(*
  vm_emulator_web.ml

  Browser interface for the Gaur VM Emulator.
*)

open Js_of_ocaml

module VE = Vm.Emulator
module E = VE.Emulate
module Ir = VE.Ir
module State = VE.State
module Inspect = State.Inspect
module PP = State.PP
module Er = VE.Err
module Vm2ir = VE.Vm2ir
module S = Vm.Syntax.Emulator
module Ast = S.Ast
module PE = S.Parsing.Err
module W = Types.Word
module DU = Dom_utils
module M = Misc

module STbl = Hashtbl.Make (String)
type src_tbl = string STbl.t

(* =========================================================================
   Standard Library Embedded Sources
   ========================================================================= *)

let math_vm_source = {|
(*******************************************************)
(*                                                     *)
(*               Math-related functions                *)
(*                                                     *)
(*******************************************************)

function Math.min 0
    push argument 0
    push argument 1
    lt
    if-goto x_case
    push argument 1
    return
    label x_case
    push argument 0
    return

function Math.max 0
    push argument 0
    push argument 1
    gt
    if-goto x_case
    push argument 1
    return
    label x_case
    push argument 0
    return

function Math.bound 0
    push argument 0
    push argument 2
    call Math.max 2
    push argument 1
    call Math.min 2
    return

function Math.abs 0
    push argument 0
    push constant 0
    lt
    if-goto neg_case
    push argument 0
    return
    label neg_case
    push argument 0
    neg
    return

function Math.nat_mul 0
    push argument 0
    push constant 1
    lt
    if-goto base_case
    push argument 0
    push constant 1
    sub
    push argument 1
    call Math.nat_mul 2
    push argument 1
    add
    return
    label base_case
    push constant 0
    return

function Math.mul 1
    push argument 0
    call Math.abs 1
    push argument 1
    call Math.abs 1
    call Math.nat_mul 2
    push argument 0
    push constant 1
    lt
    push argument 1
    push constant 1
    lt
    add
    push constant 1
    neg
    eq
    if-goto invert_case
    return
    label invert_case
    neg
    return

function Math.pow 0
    push argument 1
    push constant 1
    lt
    if-goto base_case
    push argument 0
    push argument 1
    push constant 1
    sub
    call Math.pow 2
    push argument 0
    call Math.mul 2
    return
    label base_case
    push constant 1
    return

function Math.div_unsafe 0
    push argument 0
    push argument 1
    lt
    if-goto base_case
    push constant 1
    push argument 0
    push argument 1
    sub
    push argument 1
    call Math.div_unsafe 2
    add
    return
    label base_case
    push constant 0
    return

function Math.div 0
    push argument 0
    push constant 0
    lt
    if-goto x_exception_case
    push argument 1
    push constant 1
    lt
    if-goto y_exception_case
    push argument 0
    push argument 1
    call Math.div_unsafe 2
    return
    label x_exception_case
    label y_exception_case
    push constant 0
    return

function Math.mod_unsafe 0
    push argument 0
    push argument 1
    lt
    if-goto base_case
    push argument 0
    push argument 1
    sub
    push argument 1
    call Math.mod_unsafe 2
    return
    label base_case
    push argument 0
    return

function Math.mod 0
    push argument 0
    push constant 0
    lt
    if-goto x_exception_case
    push argument 1
    push constant 1
    lt
    if-goto y_exception_case
    push argument 0
    push argument 1
    call Math.mod_unsafe 2
    return
    label x_exception_case
    label y_exception_case
    push constant 0
    return
|}

let screen_vm_source = {|
(*******************************************************)
(*                                                     *)
(*              Screen-related functions               *)
(*                                                     *)
(*******************************************************)

function Screen.set_cursor 0
    push constant 0
    push constant 79
    push argument 1
    call Math.bound 3
    push constant 0
    push constant 23
    push argument 0
    call Math.bound 3
    push constant 80
    call Math.mul 2
    add
    pop static 239
    push constant 0
    return

function Screen.get_cursor_x 0
    push static 239
    push constant 80
    call Math.div 2
    return

function Screen.get_cursor_y 0
    push static 239
    push constant 80
    call Math.mod 2
    return

function Screen.cursor_up 0
    push static 239
    push constant 80
    lt
    if-goto ret
    push static 239
    push constant 80
    sub
    pop static 239
    label ret
    push constant 0
    return

function Screen.cursor_down 0
    push static 239
    push constant 1839
    gt
    if-goto ret
    push static 239
    push constant 80
    add
    pop static 239
    label ret
    push constant 0
    return

function Screen.cursor_left 0
    call Screen.get_cursor_y 0
    push constant 1
    lt
    if-goto ret
    push static 239
    push constant 1
    sub
    pop static 239
    label ret
    push constant 0
    return

function Screen.cursor_right 0
    call Screen.get_cursor_y 0
    push constant 78
    gt
    if-goto ret
    push static 239
    push constant 1
    add
    pop static 239
    label ret
    push constant 0
    return

function Screen.cursor_incr 0
    push static 239
    push constant 1918
    gt
    if-goto ret
    push static 239
    push constant 1
    add
    pop static 239
    label ret
    push constant 0
    return

function Screen.cursor_decr 0
    push static 239
    push constant 1
    lt
    if-goto ret
    push static 239
    push constant 1
    sub
    pop static 239
    label ret
    push constant 0
    return

function Screen.print_char 0
    push static 239
    push argument 0
    scall putchar
    call Screen.cursor_incr 0
    return

function Screen.newline 0
    push static 239
    push constant 1839
    gt
    if-goto ret
    call Screen.cursor_down 0
    call Screen.get_cursor_x 0
    push constant 0
    call Screen.set_cursor 2
    label ret 
    push constant 0
    return

function Screen.backspace 0
    push static 239
    push constant 32
    scall putchar
    call Screen.cursor_decr 0
    push constant 0
    return

function Screen.print_pos 0
    push argument 0
    push constant 1
    lt
    if-goto ret
    push argument 0
    push constant 10
    call Math.div 2
    call Screen.print_pos 1
    push argument 0
    push constant 10
    call Math.mod 2
    push constant 48
    add
    call Screen.print_char 1
    label ret
    push constant 0
    return

function Screen.print_int 0
    push argument 0
    push constant 0
    lt
    if-goto neg_case
    push argument 0
    push constant 0
    eq
    if-goto zero_case
    push argument 0
    call Screen.print_pos 1
    return
    label zero_case
    push constant 48
    call Screen.print_char 1
    return
    label neg_case
    push constant 45
    call Screen.print_char 1
    push argument 0
    neg
    call Screen.print_pos 1
    return
|}

let default_vm_source = {|(* Printing "Hello" on the screen. *)

function main 0
    push constant 'H'
    call Screen.print_char 1
    push constant 'e'
    call Screen.print_char 1
    push constant 'l'
    call Screen.print_char 1
    push constant 'l'
    call Screen.print_char 1
    push constant 'o'
    call Screen.print_char 1

    push constant 0
    return
|}

(* =========================================================================
   Application State
   ========================================================================= *)

type app_state = {
  mutable raw_source : string;
  mutable filename : string;
  mutable stdlib_enabled : bool;
  mutable prog : Ir.annot Ir.prog option;
  mutable src_table : src_tbl;
  mutable sim_state : State.t option;
  mutable is_editing : bool;
  mutable keyboard_enabled : bool;
  mutable last_rv : W.t option option;
  mutable active_line_num : int option;
  mutable current_status : string;
  mutable is_halted : bool;
  mutable current_modal_frame : State.frame option;
}

let app : app_state = {
  raw_source = default_vm_source;
  filename = "hello.vm";
  stdlib_enabled = true;
  prog = None;
  src_table = STbl.create 5;
  sim_state = None;
  is_editing = false;
  keyboard_enabled = false;
  last_rv = None;
  active_line_num = None;
  current_status = "Ready";
  is_halted = false;
  current_modal_frame = None;
}

(* =========================================================================
   Compilation & Parsing
   ========================================================================= *)

let compile_program () =
  let tbl = STbl.create 5 in
  STbl.replace tbl app.filename app.raw_source;

  let parse_one src path =
    try
      let p, (p_path, p_src) = S.Parsing.Driver.parse_with_path src path in
      STbl.replace tbl p_path p_src;
      Ok p
    with
    | PE.LexingExn err ->
        let msg = PE.Lexer.to_string_with_src err src in
        Error (Printf.sprintf "Lexing error in %s:\n%s" path msg)
    | PE.ParsingExn err ->
        let msg = PE.Parser.to_string_with_src err src in
        Error (Printf.sprintf "Parsing error in %s:\n%s" path msg)
    | exn ->
        Error (Printf.sprintf "Error in %s: %s" path (Printexc.to_string exn))
  in

  match parse_one app.raw_source app.filename with
  | Error msg -> Error msg
  | Ok user_prog ->
      let progs =
        if app.stdlib_enabled then begin
          STbl.replace tbl "math.vm" math_vm_source;
          STbl.replace tbl "screen.vm" screen_vm_source;
          match parse_one math_vm_source "math.vm", parse_one screen_vm_source "screen.vm" with
          | Ok math_p, Ok screen_p -> Ok [math_p; screen_p; user_prog]
          | Error e, _ | _, Error e -> Error e
        end else
          Ok [user_prog]
      in
      match progs with
      | Error msg -> Error msg
      | Ok ps ->
          try
            let ip = Vm2ir.translate_progs "main" ps in
            app.src_table <- tbl;
            app.prog <- Some ip;
            let initial_s = E.init_from_prog ip in
            app.sim_state <- Some initial_s;
            app.is_halted <- false;
            app.last_rv <- None;
            Ok ()
          with
          | Er.SemanticError (a, e) ->
              let msg = Er.pp_semantic_error tbl (a, e) in
              Error ("Semantic error:\n" ^ msg)
          | exn ->
              Error ("Compilation error: " ^ Printexc.to_string exn)

(* =========================================================================
   UI Synchronizers & Rendering
   ========================================================================= *)

let clear_element_children el =
  while Js.to_bool el##hasChildNodes do
    match Js.Opt.to_option el##.firstChild with
    | Some child -> ignore (el##removeChild child)
    | None -> ()
  done

let sync_active_line () =
  match app.prog, app.sim_state with
  | Some prog, Some s when not app.is_halted ->
      (try
         let ins = E.fetch_inst prog s in
         let (pos_start, _) = Ir.inst_annot ins in
         if pos_start.Lexing.pos_fname = app.filename then
           app.active_line_num <- Some pos_start.Lexing.pos_lnum
         else
           app.active_line_num <- None
       with _ ->
         app.active_line_num <- None)
  | _ ->
      app.active_line_num <- None

let () =
  Dom_html.window##.onload := Dom_html.handler (fun _ ->
  let doc = Dom_html.document in

  (* Elements *)
  let prog_file_input = DU.get_input "prog-file-input" in
  let upload_prog_btn = DU.get_element "upload-prog-btn" in
  let file_name_label = DU.get_element "file-name-label" in
  let stdlib_toggle = DU.get_input "stdlib-toggle" in

  let edit_btn = DU.get_element "edit-btn" in
  let emulate_btn = DU.get_element "emulate-btn" in
  let line_numbers = DU.get_element "line-numbers" in
  let source_textarea = DU.get_textarea "source-textarea" in
  let source_viewer = DU.get_element "source-viewer" in
  let vm_error_box = DU.get_element "vm-error-box" in

  let screen_display = DU.get_element "screen-display" in
  let key_pressed_val = DU.get_element "key-pressed-val" in
  let enable_keyboard_toggle = DU.get_input "enable-keyboard-toggle" in

  let step_btn = DU.get_element "step-btn" in
  let step_n_btn = DU.get_element "step-n-btn" in
  let step_n_count = DU.get_input "step-n-count" in
  let run_btn = DU.get_element "run-btn" in
  let reset_btn = DU.get_element "reset-btn" in
  let till_return_btn = DU.get_element "till-return-btn" in
  let till_before_return_btn = DU.get_element "till-before-return-btn" in
  let next_bpt_btn = DU.get_element "next-bpt-btn" in
  let till_named_bpt_btn = DU.get_element "till-named-bpt-btn" in
  let bpt_name_input = DU.get_input "bpt-name-input" in

  let status_text = DU.get_element "status-text" in
  let rv_text = DU.get_element "rv-text" in

  let static_preview_card = DU.get_element "static-preview-card" in
  let static_preview_cells = DU.get_element "static-preview-cells" in
  let temp_preview_card = DU.get_element "temp-preview-card" in
  let temp_preview_cells = DU.get_element "temp-preview-cells" in
  let call_stack_list = DU.get_element "call-stack-list" in

  let frame_modal = DU.get_element "frame-modal" in
  let frame_modal_title = DU.get_element "frame-modal-title" in
  let close_frame_modal = DU.get_element "close-frame-modal" in
  let frame_arg_preview = DU.get_element "frame-arg-preview" in
  let frame_arg_text = DU.get_element "frame-arg-text" in
  let frame_local_preview = DU.get_element "frame-local-preview" in
  let frame_local_text = DU.get_element "frame-local-text" in
  let frame_wstack_list = DU.get_element "frame-wstack-list" in

  let segment_modal = DU.get_element "segment-modal" in
  let segment_modal_title = DU.get_element "segment-modal-title" in
  let close_segment_modal = DU.get_element "close-segment-modal" in
  let segment_modal_grid = DU.get_element "segment-modal-grid" in

  (* Modal Open/Close helpers *)
  let open_modal el =
    el##.style##.display := Js.string "flex"
  in
  let close_modal el =
    el##.style##.display := Js.string "none"
  in

  close_frame_modal##.onclick := Dom_html.handler (fun _ ->
    close_modal frame_modal;
    Js._false
  );

  close_segment_modal##.onclick := Dom_html.handler (fun _ ->
    close_modal segment_modal;
    Js._false
  );

  frame_modal##.onclick := Dom_html.handler (fun ev ->
    let target = Dom.eventTarget ev in
    if target == (frame_modal :> Dom_html.element Js.t) then
      close_modal frame_modal;
    Js._true
  );

  segment_modal##.onclick := Dom_html.handler (fun ev ->
    let target = Dom.eventTarget ev in
    if target == (segment_modal :> Dom_html.element Js.t) then
      close_modal segment_modal;
    Js._true
  );

  (* Show segment modal for a given segment *)
  let show_segment_modal title (seg : W.t array) =
    DU.set_text segment_modal_title title;
    clear_element_children segment_modal_grid;
    let n = Array.length seg in
    for i = 0 to n - 1 do
      let cell = doc##createElement (Js.string "div") in
      cell##.className := Js.string "segment-cell-item";
      let v = seg.(i) in
      let idx_span = doc##createElement (Js.string "span") in
      idx_span##.className := Js.string "cell-index";
      DU.set_text idx_span (Printf.sprintf "[%d]" i);
      let val_span = doc##createElement (Js.string "span") in
      val_span##.className := Js.string (if v <> 0l then "cell-val non-zero" else "cell-val");
      DU.set_text val_span (Printf.sprintf "%ld" v);
      Dom.appendChild cell idx_span;
      Dom.appendChild cell val_span;
      Dom.appendChild segment_modal_grid cell
    done;
    open_modal segment_modal
  in

  (* Render Frame Modal *)
  let show_frame_modal (f : State.frame) =
    app.current_modal_frame <- Some f;
    let call_str = PP.call (Inspect.call_of_frame f) in
    DU.set_text frame_modal_title call_str;

    let nargs = Array.length f.argument in
    let arg_preview =
      if nargs = 0 then "argument (0): []"
      else
        let sample = Array.to_list (Array.sub f.argument 0 (min 4 nargs)) in
        let s = String.concat ", " (List.map Int32.to_string sample) in
        Printf.sprintf "argument (%d): [ %s%s ]" nargs s (if nargs > 4 then ", ..." else "")
    in
    DU.set_text frame_arg_text arg_preview;

    let nlocals = Array.length f.local in
    let local_preview =
      if nlocals = 0 then "local (0): []"
      else
        let sample = Array.to_list (Array.sub f.local 0 (min 4 nlocals)) in
        let s = String.concat ", " (List.map Int32.to_string sample) in
        Printf.sprintf "local (%d): [ %s%s ]" nlocals s (if nlocals > 4 then ", ..." else "")
    in
    DU.set_text frame_local_text local_preview;

    clear_element_children frame_wstack_list;
    (match f.wstack with
     | [] ->
         let item = doc##createElement (Js.string "div") in
         item##.className := Js.string "stack-item";
         item##.style##.color := Js.string "var(--text-muted)";
         DU.set_text item "(Working stack is empty)";
         Dom.appendChild frame_wstack_list item
     | items ->
         List.iteri (fun idx v ->
           let item = doc##createElement (Js.string "div") in
           item##.className := Js.string (if idx = 0 then "stack-item stack-item-top" else "stack-item");
           let label = doc##createElement (Js.string "span") in
           label##.className := Js.string "stack-index";
           DU.set_text label (string_of_int idx);
           let val_span = doc##createElement (Js.string "span") in
           val_span##.className := Js.string "stack-val";
           DU.set_text val_span (Printf.sprintf "%ld" v);
           let hex_span = doc##createElement (Js.string "span") in
           hex_span##.className := Js.string "stack-hex";
           DU.set_text hex_span (Printf.sprintf "(0x%lx)" v);
           Dom.appendChild item label;
           Dom.appendChild item val_span;
           Dom.appendChild item hex_span;
           Dom.appendChild frame_wstack_list item
         ) items
    );
    open_modal frame_modal
  in

  frame_arg_preview##.onclick := Dom_html.handler (fun _ ->
    (match app.current_modal_frame with
     | Some f -> show_segment_modal (Printf.sprintf "Arguments: %s (%d words)" f.f_name (Array.length f.argument)) f.argument
     | None -> ());
    Js._false
  );

  frame_local_preview##.onclick := Dom_html.handler (fun _ ->
    (match app.current_modal_frame with
     | Some f -> show_segment_modal (Printf.sprintf "Locals: %s (%d words)" f.f_name (Array.length f.local)) f.local
     | None -> ());
    Js._false
  );

  static_preview_card##.onclick := Dom_html.handler (fun _ ->
    (match app.sim_state with
     | Some s -> show_segment_modal "Static Segment (240 words)" s.static
     | None -> ());
    Js._false
  );

  temp_preview_card##.onclick := Dom_html.handler (fun _ ->
    (match app.sim_state with
     | Some s -> show_segment_modal "Temp Segment (16 words)" s.temp
     | None -> ());
    Js._false
  );

  (* Render Segment Preview Cells *)
  let render_segment_preview container (seg : W.t array) max_cells =
    clear_element_children container;
    let n = Array.length seg in
    let count = min n max_cells in
    for i = 0 to count - 1 do
      let pill = doc##createElement (Js.string "span") in
      pill##.className := Js.string "cell-pill";
      let v = seg.(i) in
      DU.set_text pill (Printf.sprintf "%ld" v);
      Dom.appendChild container pill
    done;
    if n > max_cells then begin
      let more = doc##createElement (Js.string "span") in
      more##.className := Js.string "cell-pill more-pill";
      DU.set_text more "...";
      Dom.appendChild container more
    end
  in

  (* Render Source Viewer with Line Highlighting *)
  let render_source_viewer () =
    clear_element_children source_viewer;
    let lines = String.split_on_char '\n' app.raw_source in
    List.iteri (fun idx line_text ->
      let line_num = idx + 1 in
      let line_div = doc##createElement (Js.string "div") in
      let is_active = match app.active_line_num with Some l -> l = line_num | None -> false in
      line_div##.className := Js.string (if is_active then "code-line active-line" else "code-line");
      line_div##.id := Js.string (Printf.sprintf "code-line-%d" line_num);
      let num_span = doc##createElement (Js.string "span") in
      num_span##.className := Js.string "line-num";
      num_span##.style##.marginRight := Js.string "12px";
      num_span##.style##.color := Js.string "var(--text-muted)";
      num_span##.style##.display := Js.string "inline-block";
      num_span##.style##.width := Js.string "28px";
      num_span##.style##.textAlign := Js.string "right";
      num_span##.style##.userSelect := Js.string "none";
      DU.set_text num_span (string_of_int line_num);

      let text_span = doc##createElement (Js.string "span") in
      DU.set_text text_span (if line_text = "" then " " else line_text);

      Dom.appendChild line_div num_span;
      Dom.appendChild line_div text_span;
      Dom.appendChild source_viewer line_div;

      if is_active then
        ignore (line_div##scrollIntoView Js._false)
    ) lines
  in

  (* Render Call Stack *)
  let render_call_stack (s : State.t) =
    clear_element_children call_stack_list;
    (* In s.frames: head is the active (top) frame, tail are callers up to main *)
    let frames_top_to_bottom = List.rev s.frames in
    let active_frame = match s.frames with f :: _ -> Some f | [] -> None in

    List.iter (fun (f : State.frame) ->
      let btn = doc##createElement (Js.string "button") in
      let is_active = match active_frame with Some af -> af == f | None -> false in
      btn##.className := Js.string (if is_active then "frame-btn active-frame" else "frame-btn");
      let call_str = PP.call (Inspect.call_of_frame f) in
      let text_span = doc##createElement (Js.string "span") in
      DU.set_text text_span call_str;
      Dom.appendChild btn text_span;

      if is_active then begin
        let tag = doc##createElement (Js.string "span") in
        tag##.className := Js.string "frame-tag-active";
        DU.set_text tag "ACTIVE";
        Dom.appendChild btn tag
      end;

      btn##.onclick := Dom_html.handler (fun _ ->
        show_frame_modal f;
        Js._false
      );

      Dom.appendChild call_stack_list btn
    ) frames_top_to_bottom
  in

  (* Sync Entire UI *)
  let sync_ui () =
    DU.set_text status_text app.current_status;

    (match app.last_rv with
     | Some (Some rv) ->
         rv_text##.style##.display := Js.string "inline";
         DU.set_text rv_text (Printf.sprintf "Returned: %ld" rv)
     | Some None ->
         rv_text##.style##.display := Js.string "inline";
         DU.set_text rv_text "Terminated (no rv)"
     | None ->
         rv_text##.style##.display := Js.string "none";
         DU.clear_text rv_text
    );

    (match app.sim_state with
     | Some s ->
         (* Screen *)
         let screen_str = PP.screen_no_border s in
         DU.set_text screen_display screen_str;

         (* Keyboard *)
         (match s.keyboard with
          | Some c -> DU.set_text key_pressed_val (Printf.sprintf "%C" c)
          | None -> DU.set_text key_pressed_val "none");

         (* Segment previews *)
         render_segment_preview static_preview_cells s.static 6;
         render_segment_preview temp_preview_cells s.temp 6;

         (* Call stack *)
         render_call_stack s;

         (* Active line in source *)
         sync_active_line ();
         if not app.is_editing then render_source_viewer ()

     | None ->
         DU.set_text screen_display "(Simulation state not initialized)";
         DU.set_text key_pressed_val "none";
         clear_element_children static_preview_cells;
         clear_element_children temp_preview_cells;
         clear_element_children call_stack_list
    )
  in

  let clear_error () =
    DU.clear_text vm_error_box;
    DU.hide_element vm_error_box
  in

  let report_exec_error a e =
    let msg = Er.pp_execution_error app.src_table (a, e) in
    DU.set_text vm_error_box ("Execution error:\n" ^ msg);
    DU.show_element vm_error_box;
    app.current_status <- "Execution error (see details in source pane)"
  in

  let switch_to_edit_mode () =
    app.is_editing <- true;
    app.sim_state <- None;
    app.active_line_num <- None;
    app.is_halted <- false;
    app.last_rv <- None;
    app.current_status <- "Editing source (simulation reset)";
    clear_error ();
    DU.hide_element source_viewer;
    DU.show_element source_textarea;
    DU.show_element line_numbers;
    DU.hide_element edit_btn;
    DU.show_element emulate_btn;
    source_textarea##.value := Js.string app.raw_source;
    DU.update_line_numbers source_textarea line_numbers;
    sync_ui ()
  in

  let switch_to_emulate_mode () =
    app.raw_source <- Js.to_string source_textarea##.value;
    match compile_program () with
    | Ok () ->
        app.is_editing <- false;
        clear_error ();
        DU.hide_element source_textarea;
        DU.hide_element line_numbers;
        DU.show_element source_viewer;
        DU.show_element edit_btn;
        DU.hide_element emulate_btn;
        app.current_status <- "Ready";
        sync_ui ()
    | Error err_msg ->
        DU.set_text vm_error_box err_msg;
        DU.show_element vm_error_box;
        app.current_status <- "Compilation error (see details in source pane)";
        DU.set_text status_text app.current_status
  in

  (* Execution Handlers *)
  let handle_step () =
    if app.is_halted then
      app.current_status <- "Execution already terminated. Click Reset to run again."
    else
      match app.prog, app.sim_state with
      | Some p, Some s ->
          (try
             let ins = E.fetch_inst p s in
             let s' = E.execute_inst p s ins in
             clear_error ();
             app.sim_state <- Some s';
             app.current_status <- "Executed 1 instruction"
           with
           | Er.ExecutionError (_, (s', Er.Execution.ExecutionTerminated v)) ->
               clear_error ();
               app.sim_state <- Some s';
               app.is_halted <- true;
               app.last_rv <- Some (Some v);
               app.current_status <- Printf.sprintf "Execution terminated with return value %ld" v
           | Er.ExecutionError (a, e) ->
               app.sim_state <- Some (Er.Execution.state e);
               report_exec_error a e
           | Er.ReplError _ as exn ->
               app.current_status <- Printexc.to_string exn
           | exn ->
               app.current_status <- "Error: " ^ Printexc.to_string exn);
          sync_ui ()
      | _ -> ()
  in

  let handle_step_n count =
    if app.is_halted then
      app.current_status <- "Execution already terminated. Click Reset to run again."
    else
      match app.prog, app.sim_state with
      | Some p, Some s ->
          let rec loop current_step current_s =
            if current_step >= count then begin
              clear_error ();
              app.sim_state <- Some current_s;
              app.current_status <- Printf.sprintf "Executed %d instructions" count
            end else
              try
                let ins = E.fetch_inst p current_s in
                let next_s = E.execute_inst p current_s ins in
                loop (current_step + 1) next_s
              with
              | Er.ExecutionError (_, (s', Er.Execution.ExecutionTerminated v)) ->
                  clear_error ();
                  app.sim_state <- Some s';
                  app.is_halted <- true;
                  app.last_rv <- Some (Some v);
                  app.current_status <- Printf.sprintf "Execution terminated with return value %ld (after %d steps)" v (current_step + 1)
              | Er.ExecutionError (a, e) ->
                  app.sim_state <- Some (Er.Execution.state e);
                  report_exec_error a e
              | exn ->
                  app.sim_state <- Some current_s;
                  app.current_status <- "Error: " ^ Printexc.to_string exn
          in
          loop 0 s;
          sync_ui ()
      | _ -> ()
  in

  let handle_run () =
    if app.is_halted then
      app.current_status <- "Execution already terminated. Click Reset to run again."
    else
      match app.prog, app.sim_state with
      | Some p, Some s ->
          let max_steps = 200000 in
          let rec loop steps current_s =
            if steps >= max_steps then begin
              app.sim_state <- Some current_s;
              app.current_status <- Printf.sprintf "Reached step limit (%d steps)" max_steps
            end else
              try
                let ins = E.fetch_inst p current_s in
                let next_s = E.execute_inst p current_s ins in
                loop (steps + 1) next_s
              with
              | Er.ExecutionError (_, (s', Er.Execution.ExecutionTerminated v)) ->
                  clear_error ();
                  app.sim_state <- Some s';
                  app.is_halted <- true;
                  app.last_rv <- Some (Some v);
                  app.current_status <- Printf.sprintf "Execution terminated with return value %ld" v
              | Er.ExecutionError (a, e) ->
                  app.sim_state <- Some (Er.Execution.state e);
                  report_exec_error a e
              | exn ->
                  app.sim_state <- Some current_s;
                  app.current_status <- "Error: " ^ Printexc.to_string exn
          in
          loop 0 s;
          sync_ui ()
      | _ -> ()
  in

  let handle_reset () =
    clear_error ();
    match app.prog with
    | Some p ->
        app.sim_state <- Some (E.init_from_prog p);
        app.is_halted <- false;
        app.last_rv <- None;
        app.active_line_num <- None;
        app.current_status <- "Reset emulator state";
        sync_ui ()
    | None ->
        switch_to_emulate_mode ()
  in

  let handle_till_return () =
    if app.is_halted then
      app.current_status <- "Execution already terminated."
    else
      match app.prog, app.sim_state with
      | Some p, Some s ->
          (try
             if E.n_frames s = 1 then
               app.current_status <- "Cannot execute 'till return' from main function (already at top level)"
             else
               let call_str = match Inspect.call_of_top_frame s with
                 | Some c -> PP.call c
                 | None -> "function"
               in
               let s' = E.e_till_return p s in
               clear_error ();
               app.sim_state <- Some s';
               app.current_status <- "Finished executing " ^ call_str
           with
           | Er.ExecutionError (_, (s', Er.Execution.ExecutionTerminated v)) ->
               clear_error ();
               app.sim_state <- Some s';
               app.is_halted <- true;
               app.last_rv <- Some (Some v);
               app.current_status <- Printf.sprintf "Execution terminated with return value %ld" v
           | Er.ExecutionError (a, e) ->
               app.sim_state <- Some (Er.Execution.state e);
               report_exec_error a e
           | Er.ReplError _ as exn ->
               app.current_status <- Printexc.to_string exn
           | exn ->
               app.current_status <- "Error: " ^ Printexc.to_string exn);
          sync_ui ()
      | _ -> ()
  in

  let handle_till_before_return () =
    if app.is_halted then
      app.current_status <- "Execution already terminated."
    else
      match app.prog, app.sim_state with
      | Some p, Some s ->
          (try
             let call_str = match Inspect.call_of_top_frame s with
               | Some c -> PP.call c
               | None -> "function"
             in
             let s' = E.e_till_before_return p s in
             clear_error ();
             app.sim_state <- Some s';
             app.current_status <- call_str ^ " ready to return"
           with
           | Er.ExecutionError (_, (s', Er.Execution.ExecutionTerminated v)) ->
               clear_error ();
               app.sim_state <- Some s';
               app.is_halted <- true;
               app.last_rv <- Some (Some v);
               app.current_status <- Printf.sprintf "Execution terminated with return value %ld" v
           | Er.ExecutionError (a, e) ->
               app.sim_state <- Some (Er.Execution.state e);
               report_exec_error a e
           | Er.ReplError _ as exn ->
               app.current_status <- Printexc.to_string exn
           | exn ->
               app.current_status <- "Error: " ^ Printexc.to_string exn);
          sync_ui ()
      | _ -> ()
  in

  let handle_next_bpt () =
    if app.is_halted then
      app.current_status <- "Execution already terminated."
    else
      match app.prog, app.sim_state with
      | Some p, Some s ->
          (try
             let s' = E.e_till_any_bpt p s in
             clear_error ();
             app.sim_state <- Some s';
             app.current_status <- "Breakpoint hit"
           with
           | Er.ExecutionError (_, (s', Er.Execution.ExecutionTerminated v)) ->
               clear_error ();
               app.sim_state <- Some s';
               app.is_halted <- true;
               app.last_rv <- Some (Some v);
               app.current_status <- Printf.sprintf "Execution terminated with return value %ld" v
           | Er.ExecutionError (a, e) ->
               app.sim_state <- Some (Er.Execution.state e);
               report_exec_error a e
           | Er.ReplError _ as exn ->
               app.current_status <- Printexc.to_string exn
           | exn ->
               app.current_status <- "Error: " ^ Printexc.to_string exn);
          sync_ui ()
      | _ -> ()
  in

  let handle_till_named_bpt bpt_name =
    if bpt_name = "" then
      app.current_status <- "Please specify a breakpoint name in the input field"
    else if app.is_halted then
      app.current_status <- "Execution already terminated."
    else
      match app.prog, app.sim_state with
      | Some p, Some s ->
          (try
             let s' = E.e_till_named_bpt bpt_name p s in
             clear_error ();
             app.sim_state <- Some s';
             app.current_status <- Printf.sprintf "Breakpoint '%s' hit" bpt_name
           with
           | Er.ExecutionError (_, (s', Er.Execution.ExecutionTerminated v)) ->
               clear_error ();
               app.sim_state <- Some s';
               app.is_halted <- true;
               app.last_rv <- Some (Some v);
               app.current_status <- Printf.sprintf "Execution terminated with return value %ld" v
           | Er.ExecutionError (a, e) ->
               app.sim_state <- Some (Er.Execution.state e);
               report_exec_error a e
           | Er.ReplError _ as exn ->
               app.current_status <- Printexc.to_string exn
           | exn ->
               app.current_status <- "Error: " ^ Printexc.to_string exn);
          sync_ui ()
      | _ -> ()
  in

  (* Button Listeners *)
  edit_btn##.onclick := Dom_html.handler (fun _ ->
    switch_to_edit_mode ();
    Js._false
  );

  emulate_btn##.onclick := Dom_html.handler (fun _ ->
    switch_to_emulate_mode ();
    Js._false
  );

  step_btn##.onclick := Dom_html.handler (fun _ ->
    handle_step ();
    Js._false
  );

  step_n_btn##.onclick := Dom_html.handler (fun _ ->
    let n = try int_of_string (Js.to_string step_n_count##.value) with _ -> 10 in
    handle_step_n (max 1 n);
    Js._false
  );

  run_btn##.onclick := Dom_html.handler (fun _ ->
    handle_run ();
    Js._false
  );

  reset_btn##.onclick := Dom_html.handler (fun _ ->
    handle_reset ();
    Js._false
  );

  till_return_btn##.onclick := Dom_html.handler (fun _ ->
    handle_till_return ();
    Js._false
  );

  till_before_return_btn##.onclick := Dom_html.handler (fun _ ->
    handle_till_before_return ();
    Js._false
  );

  next_bpt_btn##.onclick := Dom_html.handler (fun _ ->
    handle_next_bpt ();
    Js._false
  );

  till_named_bpt_btn##.onclick := Dom_html.handler (fun _ ->
    let bpt_name = String.trim (Js.to_string bpt_name_input##.value) in
    handle_till_named_bpt bpt_name;
    Js._false
  );

  upload_prog_btn##.onclick := Dom_html.handler (fun _ ->
    prog_file_input##click;
    Js._false
  );

  prog_file_input##.onchange := Dom_html.handler (fun _ ->
    (match Js.Opt.to_option prog_file_input##.files with
     | Some files when files##.length > 0 ->
         (match Js.Opt.to_option (files##item 0) with
          | Some file ->
              let fname = Js.to_string file##.name in
              DU.read_file_as_text file (function
                | Some content ->
                    app.filename <- fname;
                    app.raw_source <- content;
                    DU.set_text file_name_label fname;
                    source_textarea##.value := Js.string content;
                    DU.update_line_numbers source_textarea line_numbers;
                    switch_to_emulate_mode ()
                | None ->
                    DU.set_text vm_error_box "Unable to read selected file.";
                    DU.show_element vm_error_box
              )
          | None -> ())
     | _ -> ());
    Js._false
  );

  stdlib_toggle##.onchange := Dom_html.handler (fun _ ->
    app.stdlib_enabled <- Js.to_bool stdlib_toggle##.checked;
    switch_to_emulate_mode ();
    Js._false
  );

  enable_keyboard_toggle##.onchange := Dom_html.handler (fun _ ->
    app.keyboard_enabled <- Js.to_bool enable_keyboard_toggle##.checked;
    if not app.keyboard_enabled then begin
      match app.sim_state with
      | Some s ->
          app.sim_state <- Some (E.unset_key_pressed s);
          sync_ui ()
      | None -> ()
    end;
    Js._false
  );

  (* Global Keyboard Event Listeners for VM Keyboard Simulation *)
  Dom_html.window##.onkeydown := Dom_html.handler (fun ev ->
    let active_tag =
      match Js.Opt.to_option doc##.activeElement with
      | Some el -> Js.to_string el##.tagName##toLowerCase
      | None -> ""
    in
    let is_in_input = (active_tag = "input" || active_tag = "textarea") in

    (* Ctrl + Enter in editor triggers emulation *)
    if ev##.ctrlKey = Js._true && ev##.keyCode = 13 then begin
      Dom.preventDefault ev;
      switch_to_emulate_mode ();
      Js._false
    end else if app.keyboard_enabled && not is_in_input then begin
      match Js.Optdef.to_option ev##.key with
      | Some k ->
          let key_str = Js.to_string k in
          if String.length key_str = 1 then begin
            let c = key_str.[0] in
            match app.sim_state with
            | Some s ->
                app.sim_state <- Some (E.set_key_pressed s c);
                sync_ui ();
                Js._false
            | None -> Js._true
          end else
            Js._true
      | None -> Js._true
    end else
      Js._true
  );

  Dom_html.window##.onkeyup := Dom_html.handler (fun ev ->
    let active_tag =
      match Js.Opt.to_option doc##.activeElement with
      | Some el -> Js.to_string el##.tagName##toLowerCase
      | None -> ""
    in
    let is_in_input = (active_tag = "input" || active_tag = "textarea") in

    if app.keyboard_enabled && not is_in_input then begin
      match app.sim_state with
      | Some s ->
          app.sim_state <- Some (E.unset_key_pressed s);
          sync_ui ();
          Js._false
      | None -> Js._true
    end else
      Js._true
  );

  (* Setup editor tabs & line numbers *)
  DU.setup_line_numbers source_textarea line_numbers;

  (* Initial Compilation and UI Sync *)
  source_textarea##.value := Js.string app.raw_source;
  DU.update_line_numbers source_textarea line_numbers;
  switch_to_emulate_mode ();

  Js._false
  )
