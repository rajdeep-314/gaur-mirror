(*
  dom_utils.ml

  Helper functions for Web interface.
*)

open Js_of_ocaml

let get_element id =
  Dom_html.getElementById id

let get_textarea id =
  Js.Opt.get
    (get_element id
     |> Dom_html.CoerceTo.textarea)
    (fun () -> failwith (id ^ " is not a textarea"))

let get_textarea_value id =
  let textarea = get_textarea id in
  Js.to_string textarea##.value

let set_text element text =
  element##.textContent := Js.some (Js.string text)

let clear_text element = set_text element ""

let get_input id =
  Js.Opt.get
    (get_element id
     |> Dom_html.CoerceTo.input)
    (fun () -> failwith (id ^ " is not an input"))

let get_input_value id =
  let input = get_input id in
    Js.to_string input##.value

let get_select_value id =
  let select =
    Js.Opt.get
      (get_element id
       |> Dom_html.CoerceTo.select)
      (fun () -> failwith (id ^ " is not a select"))
  in
  Js.to_string select##.value


(* Update line number  *)
let update_line_numbers textarea line_numbers =

  let source = Js.to_string textarea##.value in
  let line_count =  List.length (String.split_on_char '\n' source) in
  let numbers = String.concat "\n" (List.init line_count (fun i -> string_of_int (i + 1))) in
  line_numbers##.textContent := Js.some (Js.string numbers)

(* Resolves tabs to 4 width spaces *)
let setup_tab_to_spaces textarea =
  textarea##.onkeydown :=
    Dom_html.handler (fun ev ->

      if ev##.keyCode = 9 then begin

        Dom.preventDefault ev;

        let start = textarea##.selectionStart in
        let finish = textarea##.selectionEnd in
        let value = Js.to_string textarea##.value in
        let before = String.sub value 0 start in
        let selected = String.sub value start (finish - start) in
        let after = String.sub value finish (String.length value - finish) in
        let replacement = "    " in

        textarea##.value :=
          Js.string (before ^ replacement ^ selected ^ after);

        textarea##setSelectionRange (start + 4) (start + 4);

        Js._false

      end else begin
        Js._true
      end
    )

(* Set up line numbers and scrolling for the editor *)
(* textarea is the input and line_numbers , is the line number area. *)
let setup_line_numbers textarea line_numbers =

  update_line_numbers textarea line_numbers;
  setup_tab_to_spaces textarea;

  (* Update line numbers whenever text changes *)
  textarea##.oninput :=
    Dom_html.handler (fun _ ->
      update_line_numbers textarea line_numbers;
      Js._false
    );

  (* Keep line numbers aligned while scrolling *)
  textarea##.onscroll :=
    Dom_html.handler (fun _ ->
      line_numbers##.scrollTop :=
        textarea##.scrollTop;

      Js._false
    )

(* Copy text from an element to the clipboard *)
let copy_text element =
  let document = Dom_html.document in

  let range =
    document##createRange
  in

  range##selectNodeContents (Js.Unsafe.coerce element);

  let selection =
    Dom_html.window##getSelection
  in

  selection##removeAllRanges;
  selection##addRange range;

  ignore (
    document##execCommand
      (Js.string "copy")
      Js._false
      Js.null
  );

  selection##removeAllRanges

(* Set up a copy button for an element *)
let setup_copy_button button element =
  button##.onclick :=
    Dom_html.handler (fun _ ->
      copy_text element;

      button##.textContent := Js.some (Js.string "✓");

      ignore (
        Dom_html.window##setTimeout
          (Js.wrap_callback (fun () ->
            button##.textContent := Js.some (Js.string "Copy")
          ))
          (Js.float 1500.)
      );

      Js._false
    )

(* Read a browser file as text *)
let read_file_as_text file callback =
  let reader = new%js File.fileReader in

  reader##.onload :=
    Dom.handler (fun _ ->

      begin
        match Js.Opt.to_option (File.CoerceTo.string reader##.result) with
        | None ->
            callback None

        | Some js_result ->
            callback (Some (Js.to_string js_result))
      end;

      Js._false
    );

  reader##readAsText file


(* Set up a file input that loads a text file into a textarea *)
let setup_file_input file_input textarea line_numbers errors =
  file_input##.onchange :=
    Dom_html.handler (fun _ ->

      begin
        match Js.Opt.to_option file_input##.files with
        | None ->
            ()

        | Some files ->
            if files##.length = 0 then
              ()
            else
              match Js.Opt.to_option (files##item 0) with
              | None ->
                  set_text errors "Unable to access selected file."

              | Some file ->
                  read_file_as_text file (function
                    | None ->
                        set_text errors "Unable to read source file."

                    | Some source ->
                        textarea##.value := Js.string source;
                        update_line_numbers textarea line_numbers
                  )
      end;

      Js._false
    )


(* Set up a download button for generated output *)
let setup_download_button button output filename errors download =
  button##.onclick :=
    Dom_html.handler (fun _ ->

      begin
        match !output with
        | None ->
            set_text errors "Nothing to download."

        | Some content ->
            download !filename content
      end;

      Js._false
    )

(* Show an HTML element *)
let show_element element =
  element##.style##.display := Js.string "block"

(* Hide an HTML element *)
let hide_element element =
  element##.style##.display := Js.string "none"