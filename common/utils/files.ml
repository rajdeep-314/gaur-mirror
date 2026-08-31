(*
    files.ml

    Utility functions related to files.
*)


(** [read_file path] reads the content of a text file at [path] and returns a
    string.
    @raise Sys_error for syscall-related errors. *)
let read_file path =
    In_channel.with_open_text path In_channel.input_all

(** [write_file out_path out_content] writes [out_content] to file at path
    [out_path]. *)
let write_file out_path out_content =
    Out_channel.with_open_text out_path
    (fun oc -> output_string oc out_content; output_char oc '\n')

(** [words_of_path p] reads bytes in the file at path [p] and pairs up four
    consecutive bytes into an [int], and returns the resulting [int list].
    @raise Invalid_argument if size of file [p] is not a multiple of four
    bytes.
    @raise Sys_error for syscall-related errors. *)
let words_of_path file_path =
    let bytes = In_channel.with_open_bin file_path In_channel.input_all in
    if String.length bytes mod 4 <> 0 then
        invalid_arg
        (Printf.sprintf "words_of_path: %s has length %d, not a multiple of 4"
         file_path (String.length bytes))
    else
    let n = String.length bytes / 4 in
    List.init n (fun i ->
        let b j = Char.code bytes.[(i * 4) + j] in
        (b 0 lsl 24) lor (b 1 lsl 16) lor (b 2 lsl 8) lor b 3)

