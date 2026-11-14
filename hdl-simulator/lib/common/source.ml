open Hdlutils

type origin =
  | Disk of { path : string }
  | Memory of { content : string }

type t =
  { name : string
  ; origin : origin
  }

let from_file path = { name = path; origin = Disk { path } }
let from_string ~name content = { name; origin = Memory { content } }
let name s = s.name
let origin s = s.origin

let get_lines source ~first ~last =
  match source.origin with
  | Memory { content } -> content |> String.split_on_char '\n' |> Io.range first last
  | Disk { path } ->
    if Sys.file_exists path
    then In_channel.with_open_text path (Io.file_range first last)
    else []
;;
