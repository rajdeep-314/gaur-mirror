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
  ; mutable time            : int
  ; mutable input_type_str  : string
  ; mutable input_type      : P.wire_typ option
  ; mutable shape           : P.wire_shape option
  ; mutable log_lines       : string list
  ; mutable busy            : bool
  ; mutable needs_recompile : bool
  ; mutable pending_chip    : string option
  ; mutable last_source     : string
  ; mutable worker          : (Js.js_string Js.t, Js.js_string Js.t) Worker.worker Js.t option
  }

let () =
  let hdl_input    = DU.get_textarea "hdl-input" in
  let line_nums    = DU.get_element "line-numbers" in
  let hdl_file     = DU.get_input "hdl-file" in
  let compile_btn  = DU.get_element "compile-btn" in
  let chip_select  = get_select "chip-select" in

  let status_el      = DU.get_element "sim-status" in
  let visual_section = DU.get_element "visual-input-section" in
  let sim_buttons_el = DU.get_element "sim-buttons" in
  let visual_el      = DU.get_element "visual-input" in
  let group_select   = get_select "bit-group-select" in
  let output_btn     = DU.get_element "btn-output" in
  let clock_btn      = DU.get_element "btn-clock" in
  let reset_btn      = DU.get_element "btn-reset" in
  let clear_btn      = DU.get_element "btn-clear" in

  let log_el       = DU.get_element "sim-result" in
  let errors_el    = DU.get_element "sim-errors" in
  let copy_output  = DU.get_element "copy-output" in
  let copy_errors  = DU.get_element "copy-errors" in

  let state =
    { chip = None
    ; time = 0
    ; input_type_str = ""
    ; input_type = None
    ; shape = None
    ; log_lines = []
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

  let update_status ?busy_msg () =
    match busy_msg with
    | Some msg ->
      DU.set_text status_el msg;
      visual_section##.classList##add (Js.string "busy");
      sim_buttons_el##.classList##add (Js.string "busy")
    | None ->
      visual_section##.classList##remove (Js.string "busy");
      sim_buttons_el##.classList##remove (Js.string "busy");
      match state.chip with
      | None ->
        DU.set_text status_el "No chip loaded. Compile GHDL source to begin."
      | Some name ->
        DU.set_text status_el
          (Printf.sprintf "Chip: %s  |  t: %d  |  Input type: %s"
             name state.time state.input_type_str)
  in

  let rec render_visual_input () =
    visual_el##.innerHTML := Js.string "";
    match state.chip, state.shape with
    | Some _, Some shape ->
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
          let rec add_groups start =
            if start < len then begin
              let stop = min len (start + group_size) in
              let grp = Dom_html.createDiv Dom_html.document in
              grp##.className := Js.string "bit-group";
              for idx = start to stop - 1 do
                let btn = Dom_html.createButton Dom_html.document in
                let sync_btn b =
                  btn##.textContent := Js.some (Js.string (if b then "1" else "0"));
                  btn##.className :=
                    Js.string (if b then "bit-toggle bit-on" else "bit-toggle")
                in
                sync_btn arr.(idx);
                btn##.onclick := Dom_html.handler (fun _ ->
                  if not state.busy then begin
                    let next = not arr.(idx) in
                    arr.(idx) <- next;
                    sync_btn next;
                    DU.clear_text errors_el;
                    send (P.Req_set_input shape)
                  end;
                  Js._false);
                Dom.appendChild grp btn
              done;
              Dom.appendChild row grp;
              add_groups stop
            end
          in
          add_groups 0;
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
      Dom.appendChild visual_el (render_shape shape)
    | _ -> ()
  in

  let append_log line =
    state.log_lines <- state.log_lines @ [ line ];
    DU.set_text log_el (String.concat "\n" state.log_lines)
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
      | P.Resp_progress { msg; pct = _ } ->
        state.busy <- true;
        update_status ~busy_msg:msg ()
      | P.Resp_loaded { chip; nands = _; time; input_type_str; input_type } ->
        state.chip <- Some chip;
        state.time <- time;
        state.input_type_str <- input_type_str;
        state.input_type <- Some input_type;
        state.shape <- Some (zero_shape_of_typ input_type);
        state.log_lines <- [];
        DU.set_text log_el "Waiting for simulation commands...";
        update_status ();
        render_visual_input ()
      | P.Resp_input_set ->
        ()
      | P.Resp_output { time; shape } ->
        state.time <- time;
        let value = string_of_shape shape in
        append_log (Printf.sprintf "t: %d\t | %s" time value);
        update_status ()
      | P.Resp_clocked { time } ->
        state.time <- time;
        append_log (Printf.sprintf "t: %d\t | [clock]" time);
        update_status ()
      | P.Resp_reset { time } ->
        state.time <- time;
        state.log_lines <- [];
        DU.set_text log_el "Waiting for simulation commands...";
        (match state.input_type with
         | Some it -> state.shape <- Some (zero_shape_of_typ it)
         | None -> ());
        update_status ();
        render_visual_input ()
      | P.Resp_error { msg; clear_session } ->
        if clear_session then begin
          state.chip <- None;
          state.input_type <- None;
          state.shape <- None;
          populate_chip_select chip_select [];
          render_visual_input ()
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

  DU.setup_line_numbers_with_shortcuts hdl_input line_nums (fun ev ->
    if ev##.keyCode = 13 && Js.to_bool ev##.ctrlKey then begin
      Dom.preventDefault ev;
      request_compile ();
      Js._false
    end else
      Js._true);

  DU.setup_file_input hdl_file hdl_input line_nums errors_el;
  DU.setup_copy_button copy_output log_el;
  DU.setup_copy_button copy_errors errors_el;

  compile_btn##.onclick := Dom_html.handler (fun _ ->
    request_compile ();
    Js._false);

  chip_select##.onchange := Dom_html.handler (fun _ ->
    let chip_name = Js.to_string chip_select##.value in
    if chip_name <> "" then request_load chip_name;
    Js._false);

  group_select##.onchange := Dom_html.handler (fun _ ->
    render_visual_input ();
    Js._false);

  output_btn##.onclick := Dom_html.handler (fun _ ->
    if not state.busy then begin
      DU.clear_text errors_el;
      if state.needs_recompile then
        DU.set_text errors_el "Worker was restarted after an error. Please re-compile or re-select a chip."
      else
        send P.Req_output
    end;
    Js._false);

  clock_btn##.onclick := Dom_html.handler (fun _ ->
    if not state.busy then begin
      DU.clear_text errors_el;
      if state.needs_recompile then
        DU.set_text errors_el "Worker was restarted after an error. Please re-compile or re-select a chip."
      else
        send P.Req_clock
    end;
    Js._false);

  reset_btn##.onclick := Dom_html.handler (fun _ ->
    if not state.busy then begin
      DU.clear_text errors_el;
      if state.needs_recompile then
        DU.set_text errors_el "Worker was restarted after an error. Please re-compile or re-select a chip."
      else
        send P.Req_reset
    end;
    Js._false);

  clear_btn##.onclick := Dom_html.handler (fun _ ->
    state.log_lines <- [];
    DU.set_text log_el "";
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
  DU.set_text log_el "Waiting for simulation commands...";
  DU.clear_text errors_el
