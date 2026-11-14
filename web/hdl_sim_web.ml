(** Browser UI for the HDL Simulator, communicating with hdl_sim_worker.bc.js. *)

open Js_of_ocaml

module DU = Dom_utils
module P  = Hdl_sim_proto

let get_select id =
  Js.Opt.get
    (DU.get_element id |> Dom_html.CoerceTo.select)
    (fun () -> failwith (id ^ " is not a select"))

let populate_chip_select select names =
  select##.innerHTML := Js.string "";
  List.iter
    (fun name ->
       let opt = Dom_html.createOption Dom_html.document in
       opt##.value := Js.string name;
       opt##.textContent := Js.some (Js.string name);
       Dom.appendChild select opt)
    names

let rec zero_shape_of_typ (t : P.wire_typ) : P.wire_shape =
  match t with
  | P.WTBits n ->
    P.WSBitvec (Array.make n false)
  | P.WTRecord fields ->
    P.WSRecord (List.map (fun (k, v) -> (k, zero_shape_of_typ v)) fields)

let rec string_of_shape = function
  | P.WSBitvec arr ->
    let bits = Array.to_list arr |> List.rev |> List.map (fun b -> if b then "1" else "0") in
    "0b" ^ String.concat "" bits
  | P.WSRecord fields ->
    "{" ^ String.concat ", " (List.map (fun (k, v) -> k ^ " = " ^ string_of_shape v) fields) ^ "}"

type ui_state =
  { mutable chip            : string option
  ; mutable nands           : int
  ; mutable time            : int
  ; mutable input_type_str  : string
  ; mutable input_type      : P.wire_typ option
  ; mutable shape           : P.wire_shape option
  ; mutable output_shape    : P.wire_shape option
  ; mutable busy            : bool
  ; mutable needs_recompile : bool
  ; mutable pending_chip    : string option
  ; mutable last_source     : string
  ; mutable worker          : (Js.js_string Js.t, Js.js_string Js.t) Worker.worker Js.t option
  }

let escape_html_into buf s start len =
  for i = start to start + len - 1 do
    match s.[i] with
    | '&' -> Buffer.add_string buf "&amp;"
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | c   -> Buffer.add_char buf c
  done

let token_re =
  Str.regexp
    "#[^\n]*\\|\\b\\(CHIP\\|INPUT\\|OUTPUT\\|STATES\\|bits\\)\\b\\|\\bNand\\b\\|\\b[A-Z][a-zA-Z0-9_]*\\b\\|\\b[0-9]+\\b\\|\\+\\+\\|<-\\|:=\\|\\."

let token_class tok =
  match tok with
  | "CHIP" | "INPUT" | "OUTPUT" | "STATES" | "bits" -> "tok-keyword"
  | "Nand" -> "tok-builtin"
  | _ ->
    match tok.[0] with
    | '#' -> "tok-comment"
    | 'A' .. 'Z' -> "tok-type"
    | '0' .. '9' -> "tok-number"
    | _ -> "tok-op"

let highlight_phdl src =
  let len = String.length src in
  let buf = Buffer.create (len + 64) in
  let rec loop pos =
    if pos >= len then ()
    else
      match Str.search_forward token_re src pos with
      | idx ->
        if idx > pos then escape_html_into buf src pos (idx - pos);
        let tok = Str.matched_string src in
        let tlen = String.length tok in
        Buffer.add_string buf "<span class=\"";
        Buffer.add_string buf (token_class tok);
        Buffer.add_string buf "\">";
        escape_html_into buf src idx tlen;
        Buffer.add_string buf "</span>";
        loop (idx + tlen)
      | exception Not_found ->
        escape_html_into buf src pos (len - pos)
  in
  loop 0;
  if len > 0 && src.[len - 1] = '\n' then Buffer.add_char buf ' ';
  Buffer.contents buf

let () =
  let hdl_input     = DU.get_textarea "hdl-input" in
  let hdl_highlight = DU.get_element "hdl-highlight" in
  let line_nums     = DU.get_element "line-numbers" in
  let hdl_file      = DU.get_input "hdl-file" in
  let compile_btn   = DU.get_element "compile-btn" in
  let chip_select   = get_select "chip-select" in

  let status_el        = DU.get_element "sim-status" in
  let timer_el         = DU.get_element "sim-timer" in
  let time_value_el    = DU.get_element "sim-time-value" in
  let progress_overlay = DU.get_element "sim-progress-overlay" in
  let progress_msg_el  = DU.get_element "sim-progress-msg" in
  let progress_bar     = DU.get_element "sim-progress-bar" in
  let visual_section   = DU.get_element "visual-input-section" in
  let sim_buttons_el   = DU.get_element "sim-buttons" in
  let visual_el        = DU.get_element "visual-input" in
  let output_el        = DU.get_element "visual-output" in
  let group_select     = get_select "bit-group-select" in
  let endian_btn       = DU.get_element "endian-toggle" in
  let is_big_endian     = ref true in
  let output_btn       = DU.get_element "btn-output" in
  let clock_btn        = DU.get_element "btn-clock" in
  let reset_btn        = DU.get_element "btn-reset" in

  let errors_el    = DU.get_element "sim-errors" in
  let copy_errors  = DU.get_element "copy-errors" in

  let state =
    { chip = None
    ; nands = 0
    ; time = 0
    ; input_type_str = ""
    ; input_type = None
    ; shape = None
    ; output_shape = None
    ; busy = false
    ; needs_recompile = false
    ; pending_chip = None
    ; last_source = ""
    ; worker = None
    }
  in

  let send (req : P.req) =
    match state.worker with
    | Some w -> w##postMessage (P.encode req)
    | None -> ()
  in

  let show_progress msg (pct : int option) =
    progress_overlay##.classList##remove (Js.string "hidden");
    DU.set_text progress_msg_el msg;
    match pct with
    | None ->
      progress_bar##.style##.width := Js.string "";
      progress_bar##.classList##add (Js.string "indeterminate")
    | Some p ->
      let p = max 0 (min 100 p) in
      progress_bar##.classList##remove (Js.string "indeterminate");
      progress_bar##.style##.width := Js.string (Printf.sprintf "%d%%" p)
  in

  let hide_progress () =
    progress_overlay##.classList##add (Js.string "hidden");
    progress_bar##.classList##remove (Js.string "indeterminate");
    progress_bar##.style##.width := Js.string "0%"
  in

  let update_timer () =
    match state.chip with
    | None ->
      timer_el##.classList##add (Js.string "hidden")
    | Some _ ->
      timer_el##.classList##remove (Js.string "hidden");
      DU.set_text time_value_el (string_of_int state.time)
  in

  let update_status ?busy_msg ?(progress : int option = None) () =
    update_timer ();
    match busy_msg with
    | Some msg ->
      show_progress msg progress;
      visual_section##.classList##add (Js.string "busy");
      sim_buttons_el##.classList##add (Js.string "busy")
    | None ->
      hide_progress ();
      visual_section##.classList##remove (Js.string "busy");
      sim_buttons_el##.classList##remove (Js.string "busy");
      match state.chip with
      | None ->
        status_el##.classList##remove (Js.string "hidden");
        DU.set_text status_el "No chip loaded. Compile GHDL source to begin."
      | Some _ ->
        status_el##.classList##add (Js.string "hidden");
        DU.set_text status_el ""
  in

  let render_shape_into ~editable container root_shape =
    let group_size =
      match int_of_string_opt (Js.to_string group_select##.value) with
      | Some n when n > 0 -> n
      | _ -> 4
    in
    let rec render_shape (v : P.wire_shape) : Dom_html.element Js.t =
      match v with
      | P.WSBitvec arr ->
        let row = Dom_html.createDiv Dom_html.document in
        row##.className := Js.string "bit-row";
        let len = Array.length arr in
        let create_tile idx =
          let end_cls =
            if len <= 1 then ""
            else if idx = 0 then " bit-low"
            else if idx = len - 1 then " bit-high"
            else ""
          in
          let tile_class b =
            let base = if b then "bit-toggle bit-on" else "bit-toggle" in
            let ro = if editable then "" else " bit-readonly" in
            base ^ ro ^ end_cls
          in
          if editable then begin
            let btn = Dom_html.createButton Dom_html.document in
            let sync_btn b =
              btn##.textContent := Js.some (Js.string (if b then "1" else "0"));
              btn##.className := Js.string (tile_class b)
            in
            sync_btn arr.(idx);
            btn##.onclick := Dom_html.handler (fun _ ->
              if not state.busy then begin
                let next = not arr.(idx) in
                arr.(idx) <- next;
                sync_btn next;
                DU.clear_text errors_el;
                send (P.Req_set_input root_shape)
              end;
              Js._false);
            (btn :> Dom_html.element Js.t)
          end else begin
            let tile = Dom_html.createSpan Dom_html.document in
            let b = arr.(idx) in
            tile##.textContent := Js.some (Js.string (if b then "1" else "0"));
            tile##.className := Js.string (tile_class b);
            (tile :> Dom_html.element Js.t)
          end
        in
        if not !is_big_endian then begin
          (* Little-endian: bit 0 (blue) on left, MSB (orange) on right *)
          let rec add_groups start =
            if start < len then begin
              let stop = min len (start + group_size) in
              let grp = Dom_html.createDiv Dom_html.document in
              grp##.className := Js.string "bit-group";
              for idx = start to stop - 1 do
                Dom.appendChild grp (create_tile idx)
              done;
              Dom.appendChild row grp;
              add_groups stop
            end
          in
          add_groups 0
        end else begin
          (* Big-endian: MSB (orange) on left, bit 0 (blue) on right *)
          let rem = len mod group_size in
          let first_size = if rem = 0 then group_size else rem in
          let rec loop hi =
            if hi >= 0 then begin
              let size = if hi = len - 1 then first_size else group_size in
              let lo = hi - size + 1 in
              let grp = Dom_html.createDiv Dom_html.document in
              grp##.className := Js.string "bit-group";
              for idx = hi downto lo do
                Dom.appendChild grp (create_tile idx)
              done;
              Dom.appendChild row grp;
              loop (lo - 1)
            end
          in
          loop (len - 1)
        end;
        (row :> Dom_html.element Js.t)
      | P.WSRecord fields ->
        let group = Dom_html.createDiv Dom_html.document in
        group##.className := Js.string "record-group";
        List.iter
          (fun (name, sub) ->
             let field_el = Dom_html.createDiv Dom_html.document in
             field_el##.className := Js.string "record-field";
             let label_el = Dom_html.createSpan Dom_html.document in
             label_el##.className := Js.string "record-label";
             label_el##.textContent := Js.some (Js.string name);
             Dom.appendChild field_el label_el;
             Dom.appendChild field_el (render_shape sub);
             Dom.appendChild group field_el)
          fields;
        (group :> Dom_html.element Js.t)
    in
    Dom.appendChild container (render_shape root_shape)
  in

  let render_visual_input () =
    visual_el##.innerHTML := Js.string "";
    match state.chip, state.shape with
    | Some _, Some shape -> render_shape_into ~editable:true visual_el shape
    | _ -> ()
  in

  let render_visual_output () =
    output_el##.innerHTML := Js.string "";
    match state.chip, state.output_shape with
    | Some _, Some shape ->
      render_shape_into ~editable:false output_el shape
    | Some _, None ->
      let msg = Dom_html.createP Dom_html.document in
      msg##.className := Js.string "output-placeholder";
      msg##.textContent :=
        Js.some (Js.string "Press Output to evaluate and display the current output.");
      Dom.appendChild output_el msg
    | None, _ -> ()
  in

  let rec respawn_worker () =
    (match state.worker with
     | Some w -> w##terminate
     | None -> ());
    let w = Worker.create "./hdl_sim_worker.bc.js" in
    w##.onmessage := Dom.handler (fun ev ->
      let resp : P.resp = P.decode ev##.data in
      state.busy <- false;
      begin match resp with
      | P.Resp_compiled { names; selected } ->
        state.needs_recompile <- false;
        populate_chip_select chip_select names;
        let target =
          match state.pending_chip with
          | Some c ->
            state.pending_chip <- None;
            Some c
          | None -> selected
        in
        (match target with
         | Some chip_name ->
           chip_select##.value := Js.string chip_name;
           state.busy <- true;
           update_status ~busy_msg:("Building " ^ chip_name ^ "...") ();
           send (P.Req_load chip_name)
         | None ->
           update_status ())
      | P.Resp_progress { msg; pct } ->
        state.busy <- true;
        update_status ~busy_msg:msg ~progress:pct ()
      | P.Resp_loaded { chip; nands; time; input_type_str; input_type } ->
        state.chip <- Some chip;
        state.nands <- nands;
        state.time <- time;
        state.input_type_str <- input_type_str;
        state.input_type <- Some input_type;
        state.shape <- Some (zero_shape_of_typ input_type);
        state.output_shape <- None;
        update_status ();
        render_visual_input ();
        render_visual_output ()
      | P.Resp_input_set ->
        ()
      | P.Resp_output { time; shape } ->
        state.time <- time;
        state.output_shape <- Some shape;
        update_status ();
        render_visual_output ()
      | P.Resp_clocked { time } ->
        state.time <- time;
        update_status ()
      | P.Resp_reset { time } ->
        state.time <- time;
        state.output_shape <- None;
        (match state.input_type with
         | Some it -> state.shape <- Some (zero_shape_of_typ it)
         | None -> ());
        update_status ();
        render_visual_input ();
        render_visual_output ()
      | P.Resp_error { msg; clear_session } ->
        if clear_session then begin
          state.chip <- None;
          state.input_type <- None;
          state.shape <- None;
          state.output_shape <- None;
          populate_chip_select chip_select [];
          render_visual_input ();
          render_visual_output ()
        end else begin
          match state.chip with
          | Some active -> chip_select##.value := Js.string active
          | None -> ()
        end;
        DU.set_text errors_el msg;
        update_status ()
      end;
      Js._false);
    w##.onerror := Dom.handler (fun ev ->
      Dom.preventDefault ev;
      state.busy <- false;
      state.needs_recompile <- true;
      state.pending_chip <- None;
      respawn_worker ();
      (match state.chip with
       | Some active -> chip_select##.value := Js.string active
       | None -> ());
      DU.set_text errors_el
        "Chip is too large to simulate in the browser (worker ran out of memory or crashed).";
      update_status ();
      Js._false);
    state.worker <- Some w
  in

  let request_compile () =
    if state.busy then respawn_worker ();
    let src = DU.get_textarea_value "hdl-input" in
    state.last_source <- src;
    state.pending_chip <- None;
    state.busy <- true;
    DU.clear_text errors_el;
    update_status ~busy_msg:"Compiling GHDL source..." ();
    send (P.Req_compile src)
  in

  let request_load chip_name =
    if state.busy then begin
      respawn_worker ();
      state.needs_recompile <- true
    end;
    DU.clear_text errors_el;
    if state.needs_recompile then begin
      state.pending_chip <- Some chip_name;
      state.busy <- true;
      update_status ~busy_msg:("Building " ^ chip_name ^ "...") ();
      send (P.Req_compile state.last_source)
    end else begin
      state.busy <- true;
      update_status ~busy_msg:("Building " ^ chip_name ^ "...") ();
      send (P.Req_load chip_name)
    end
  in

  respawn_worker ();

  DU.setup_line_numbers_with_shortcuts ~expand_placeholder:true hdl_input line_nums (fun ev ->
    if ev##.keyCode = 13 && Js.to_bool ev##.ctrlKey then begin
      Dom.preventDefault ev;
      request_compile ();
      Js._false
    end else
      Js._true);

  let update_highlight () =
    let src = Js.to_string hdl_input##.value in
    hdl_highlight##.innerHTML := Js.string (highlight_phdl src)
  in
  let sync_highlight_scroll () =
    hdl_highlight##.scrollTop := hdl_input##.scrollTop;
    hdl_highlight##.scrollLeft := hdl_input##.scrollLeft
  in
  ignore (Dom_html.addEventListener hdl_input Dom_html.Event.input
    (Dom_html.handler (fun _ -> update_highlight (); Js._true)) Js._false);
  ignore (Dom_html.addEventListener hdl_input Dom_html.Event.scroll
    (Dom_html.handler (fun _ -> sync_highlight_scroll (); Js._true)) Js._false);
  update_highlight ();

  DU.setup_file_input hdl_file hdl_input line_nums errors_el;
  DU.setup_copy_button copy_errors errors_el;

  compile_btn##.onclick := Dom_html.handler (fun _ ->
    request_compile ();
    Js._false);

  chip_select##.onchange := Dom_html.handler (fun _ ->
    let chip_name = Js.to_string chip_select##.value in
    if chip_name <> "" then request_load chip_name;
    Js._false);

  let update_endian_ui () =
    if !is_big_endian then begin
      endian_btn##.textContent := Js.some (Js.string "Big");
      endian_btn##setAttribute (Js.string "title")
        (Js.string "Click to toggle endianness (currently Big-endian: MSB on left, bit 0 on right)")
    end else begin
      endian_btn##.textContent := Js.some (Js.string "Little");
      endian_btn##setAttribute (Js.string "title")
        (Js.string "Click to toggle endianness (currently Little-endian: bit 0 on left, MSB on right)")
    end
  in

  endian_btn##.onclick := Dom_html.handler (fun _ ->
    is_big_endian := not !is_big_endian;
    update_endian_ui ();
    render_visual_input ();
    render_visual_output ();
    Js._false);

  group_select##.onchange := Dom_html.handler (fun _ ->
    render_visual_input ();
    render_visual_output ();
    Js._false);

  let run_sim_cmd busy_msg req =
    if not state.busy && state.chip <> None then begin
      DU.clear_text errors_el;
      if state.needs_recompile then
        DU.set_text errors_el "Worker was restarted after an error. Please re-compile or re-select a chip."
      else begin
        state.busy <- true;
        update_status ~busy_msg ();
        send req
      end
    end
  in

  output_btn##.onclick := Dom_html.handler (fun _ ->
    run_sim_cmd "Stabilizing output..." P.Req_output;
    Js._false);

  clock_btn##.onclick := Dom_html.handler (fun _ ->
    run_sim_cmd "Clocking circuit..." P.Req_clock;
    Js._false);

  reset_btn##.onclick := Dom_html.handler (fun _ ->
    run_sim_cmd "Resetting circuit..." P.Req_reset;
    Js._false);

  let setup_splitter ~horizontal splitter prev_el container min_pct max_pct =
    let dragging = ref false in
    let body_cls = if horizontal then "resizing-h" else "resizing-v" in
    splitter##.onmousedown := Dom_html.handler (fun ev ->
      Dom.preventDefault ev;
      dragging := true;
      splitter##.classList##add (Js.string "active");
      Dom_html.document##.body##.classList##add (Js.string body_cls);
      Js._false);
    ignore (Dom_html.addEventListener Dom_html.document Dom_html.Event.mousemove
      (Dom_html.handler (fun ev ->
         if !dragging then begin
           let rect = container##getBoundingClientRect in
           let r_top = Js.to_float rect##.top in
           let r_bottom = Js.to_float rect##.bottom in
           let r_left = Js.to_float rect##.left in
           let r_right = Js.to_float rect##.right in
           let span =
             if horizontal then r_bottom -. r_top
             else r_right -. r_left
           in
           if span > 0. then begin
             let offset =
               if horizontal then Js.to_float ev##.clientY -. r_top
               else Js.to_float ev##.clientX -. r_left
             in
             let raw = (offset /. span) *. 100. in
             let pct = max min_pct (min max_pct raw) in
             prev_el##.style##.flex := Js.string (Printf.sprintf "0 0 %.2f%%" pct)
           end
         end;
         Js._true))
      Js._false);
    ignore (Dom_html.addEventListener Dom_html.document Dom_html.Event.mouseup
      (Dom_html.handler (fun _ ->
         if !dragging then begin
           dragging := false;
           splitter##.classList##remove (Js.string "active");
           Dom_html.document##.body##.classList##remove (Js.string body_cls)
         end;
         Js._true))
      Js._false)
  in

  setup_splitter ~horizontal:false
    (DU.get_element "main-splitter")
    (DU.get_element "input-panel")
    (DU.get_element "app")
    20. 80.;
  setup_splitter ~horizontal:true
    (DU.get_element "io-splitter")
    visual_section
    (DU.get_element "io-panes")
    20. 80.;

  update_status ();
  DU.clear_text errors_el
