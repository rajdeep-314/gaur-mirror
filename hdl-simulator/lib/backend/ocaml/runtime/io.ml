let output_stringln outx s = output_string outx (s ^ "\n")

let read_line inx =
  match In_channel.input_line inx with
  | None   -> "QUIT"
  | Some s -> s
