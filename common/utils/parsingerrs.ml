(** Helper functions related to parsing (and lexing) errors. *)


open Lexing


(** Type [pos_info] is a tuple of [Lexing.position] values. *)
type pos_info = position * position


(** Pretty-printing a [pos_info] value. *)
let pp_pos_info ((p1, p2):pos_info) : string =
  let line1, col1 = p1.pos_lnum, p1.pos_cnum - p1.pos_bol in
  let line2, col2 = p2.pos_lnum, p2.pos_cnum - p2.pos_bol in
  if line1 = line2 then
    Printf.sprintf "File \"%s\", line %d, characters %d-%d"
      p1.pos_fname line1 col1 col2
  else
    Printf.sprintf "File \"%s\", lines %d-%d, characters %d-%d"
      p1.pos_fname line1 line2 col1 col2

(** [pp_pos_info_with_source ~source:src ps] generates a string of the kind
        < details about the error's location >
        < the snippet responsible for the error >
    where [src] is the source program from which the snippet is extracted and
    [ps] is a value of type [pos_info], which points to where the error was
    from. *)
let pp_pos_info_with_source ~(source : string) ((p1, p2) : pos_info) : string =
  let line1, col1 = p1.pos_lnum, p1.pos_cnum - p1.pos_bol in
  let line2, col2 = p2.pos_lnum, p2.pos_cnum - p2.pos_bol in
  let lines = Array.of_list (String.split_on_char '\n' source) in
  let get_line n = try lines.(n - 1) with _ -> "" in
  let header = pp_pos_info (p1, p2) in
  let gutter n = Printf.sprintf "%d | " n in
  let marker gutter_width col = String.make (gutter_width + col) ' ' ^ "*" in
  if line1 = line2 then
    let g = gutter line1 in
    let underline = String.make (max 1 (col2 - col1)) '^' in
    Printf.sprintf "%s:\n%s%s\n%s%s"
      header g (get_line line1) (String.make (String.length g) ' ' ^ String.make col1 ' ') underline
  else
    let body =
      List.init (line2 - line1 + 1) (fun i ->
        let n = line1 + i in
        let g = gutter n in
        let text = Printf.sprintf "%s%s" g (get_line n) in
        if n = line1 then Printf.sprintf "%s\n%s" (marker (String.length g) col1) text
        else if n = line2 then Printf.sprintf "%s\n%s" text (marker (String.length g) col2)
        else text)
      |> String.concat "\n"
    in
    Printf.sprintf "%s:\n%s" header body

