open Raminit

let args = Sys.argv

let file_path = args.(1)

let maps =
    let inc = open_in file_path in
    really_input_string inc (in_channel_length inc)

let ms = Parser.parse_maps maps

let bs = Translate.translate_maps ms

let () = Binary.print bs

