(*
  misc.ml

  Contains miscellaneous functions
*)

open Js_of_ocaml

module T = Typed_array

(* Convert an ArrayBuffer containing 32-bit big-endian words
   into the same representation as Utils.Files.words_of_path. *)
let words_of_array_buffer buffer =
  let bytes =
    new%js T.uint8Array_fromBuffer buffer
  in

  let length = bytes##.length in

  if length mod 4 <> 0 then Error length
  else
    let get_byte i = T.unsafe_get bytes i in

    let rec loop i acc =
      if i >= length then
        Ok (List.rev acc)
      else
        let b1 = get_byte i in
        let b2 = get_byte (i + 1) in
        let b3 = get_byte (i + 2) in
        let b4 = get_byte (i + 3) in

        let instr =
          (b1 lsl 24) lor
          (b2 lsl 16) lor
          (b3 lsl 8) lor
          b4
        in

        loop (i + 4) (instr :: acc)
    in

    loop 0 []


(*   Download binary output    *)
let download_file filename content =

  (* Convert OCaml string containing raw bytes
     into a JavaScript Uint8Array. *)
  let bytes =
    Bytes.of_string content
  in

  let uint8 =
    Typed_array.Bytes.to_uint8Array bytes
  in

  (* Create Blob from the Uint8Array. *)
  let blob =
    Js.Unsafe.new_obj
      (Js.Unsafe.pure_js_expr "Blob")
      [|
        Js.Unsafe.inject
          (Js.array
             [| Js.Unsafe.inject uint8 |])
      |]
  in

  let url =
    Dom_html.window##._URL##createObjectURL blob
  in

  let link =
    Dom_html.createA Dom_html.document
  in

  link##.href := url;
  link##.download := Js.string filename;

  Dom.appendChild
    Dom_html.document##.body
    link;

  link##click;

  Dom.removeChild
    Dom_html.document##.body
    link;

  Dom_html.window##._URL##revokeObjectURL url


(* Produce an xxd-like hexadecimal dump *)
let hex_dump content =
  let len = String.length content in

  let ascii_char c =
    let code = Char.code c in
    if code >= 32 && code <= 126 then
      c
    else
      '.'
  in

  let make_line offset =
    let count = min 16 (len - offset) in

    let hex =
      String.concat " "
        (List.init 16 (fun i ->
          if i < count then
            Printf.sprintf "%02x" (Char.code content.[offset + i])
          else
            "  "
        ))
    in

    let ascii =
      String.init count (fun i ->
        ascii_char content.[offset + i]
      )
    in

    Printf.sprintf
      "%08x  %s  |%s|"
      offset
      hex
      ascii
  in

  let rec loop offset acc =
    if offset >= len then
      String.concat "\n" (List.rev acc)
    else
      loop
        (offset + 16)
        (make_line offset :: acc)
  in

  loop 0 []