open Parse

let string = "> "

let rec read outx () =
  output_string outx string;
  let s = read_line () in
  try parse s with
  | Errors.Error err ->
    Io.output_stringln outx (Fmt.to_to_string Errors.Pretty.pp err);
    read outx ()

  | Parser.Error st ->
    Io.output_stringln outx (Messages.message st);
    read outx ()
