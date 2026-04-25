(** [argc] is the number of arguments passed to the executable. *)
let argc = Array.length Sys.argv

(** [usage s] prompts a help text [s] if no arguments are provided to
    the executable.
*)
let usage s =
  if argc <= 1
  then begin
    prerr_endline "Usage: ";
    output_string stderr s;
    prerr_endline "";
    exit Exit.usage
  end

let output_stringln outx s = output_string outx (s ^ "\n")
let output_string = output_string
