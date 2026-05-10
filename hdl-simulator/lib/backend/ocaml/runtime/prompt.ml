open Parse

let string = "> "
let show_prompt outx =
  output_string outx string;
  Out_channel.flush outx

let rec read inx outx ?(exiterr=false) () =
  let s = Io.read_line inx in
  match String.trim s with
  | "" -> read inx outx ~exiterr:exiterr ()
  | _  ->
    (try parse s with
    | Errors.Error err ->
      Io.output_stringln outx (Fmt.to_to_string Errors.Pretty.pp err);
      if exiterr then exit 1;
      read inx outx ~exiterr:exiterr ()

    | Parser.Error st ->
      Io.output_stringln stderr (Messages.message st);
      read inx outx ~exiterr:exiterr ())
