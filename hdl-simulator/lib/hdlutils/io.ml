(** [argc] is the number of arguments passed to the executable. *)
let argc () = Array.length Sys.argv

(** [make_usage_text msg] generates usage text for [msg].*)
let make_usage_msg msg =
  let basename = Filename.basename Sys.argv.(0) in
  "Usage: " ^ basename ^ " " ^ msg

(** [usage s] prompts a help text [s] if no arguments are provided to
    the executable.
*)
let usage s =
  if argc () <= 1
  then begin
    let msg = make_usage_msg s in
    prerr_endline msg;
    exit Exit.usage
  end

(** [read_files] returns a list of [(fname, inx)], file names
    from the argument list and their in-channels.
*)
let read_files () =
  Sys.argv
  |> Array.to_list
  |> List.tl
  |> List.map (fun s -> s, In_channel.open_text s)

(** [open_in_many [file1, ...]] opens in-channels for all files
    [file1, ...].
*)
let open_in_many sl =
  sl
  |> List.map (fun s -> s, In_channel.open_text s)

(** [close_in_many [inx, ...]] closes all in-channels [inx, ...]. *)
let close_in_many icl =
  icl
  |> List.map In_channel.close

(** [range fst lst ip] returns the list of lines of [ip] between
    line number [fst] untill [lst] with respective line numbers. *)
let range fst lst ip =
  ip
  |> List.mapi (fun i x -> (i + 1, x))
  |> List.drop (fst - 1)
  |> List.take (lst - fst + 1)

(** [file_range fst lst ip] returns the list of lines of in channel
    [ic] between line number [fst] untill [lst] with respective line
    numbers. *)
let file_range fst lst ic =
  ic
  |> In_channel.input_lines
  |> range fst lst
