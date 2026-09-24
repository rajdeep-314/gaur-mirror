(*
  web_utils.ml

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


