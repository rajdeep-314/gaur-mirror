(** [argc] is the number of arguments passed to the executable. *)
let argc = Array.length Sys.argv

(** [usage s] prompts a help text [s] if no arguments are provided to
    the executable.
*)
let usage s =
  if argc <= 1
  then begin
    let basename = Filename.basename Sys.argv.(0) in
    let msg = "Usage: " ^ basename ^ " " ^ s ^ "\n" in
    output_string stderr msg;
    exit Exit.usage
  end

(** [read_files] returns a list of [(fname, inx)], file names
    from the argument list and their in-channels.
*)
let read_files =
  Sys.argv
  |> Array.to_list
  |> List.tl
  |> List.map (fun s -> s, In_channel.open_text s)

(** [close_many [inx, ...]] closes all in-channels [inx, ...]. *)
let close_many icl =
  icl
  |> List.map In_channel.close
