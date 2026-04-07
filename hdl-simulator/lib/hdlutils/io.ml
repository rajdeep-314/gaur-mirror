(** [argc] is the number of arguments passed to the executable. *)
let argc = Array.length Sys.argv

(** [usage s] prompts a help text [s] if no arguments are provided to
    the executable.
*)
let usage s =
  if argc = 0
  then
    output_string stderr "Usage: \n";
    output_string stderr s;
    output_string stderr "\n"
