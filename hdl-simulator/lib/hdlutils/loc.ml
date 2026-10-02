type t =
  { loc_start : Lexing.position
  ; loc_end   : Lexing.position
  }

let dummy =
  { loc_start=Lexing.dummy_pos
  ; loc_end  =Lexing.dummy_pos
  }

type 'a wrapped =
  { txt : 'a
  ; loc : t
  }

let make loc_start loc_end =
  { loc_start=loc_start;
    loc_end=loc_end
  }

let wrap ~loc txt =
  { txt=txt;
    loc=loc
  }

let range loc =
  let pos1 = loc.loc_start in
  let pos2 = loc.loc_end in
  let file = pos1.pos_fname in
  let line = pos1.pos_lnum in
  let char1 = pos1.pos_cnum - pos1.pos_bol in
  let char2 = pos2.pos_cnum - pos1.pos_bol in (* yes, [pos1.pos_bol] *)
  Printf.sprintf "File \"%s\", line %d, characters %d-%d:"
    file line (char1 + 1) (char2 + 1)
(* use [char1 + 1] and [char2 + 1] if *not* using Caml mode *)

let location (lexbuf : Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  let char = (pos.pos_cnum - pos.pos_bol + 1) in
  Printf.sprintf "File \"%s\", line %d, character %d:\n"
    pos.pos_fname
    pos.pos_lnum
    char

let errorf ~loc fmt =
  output_string stderr (range loc ^ "\n");
  Printf.eprintf "Error: ";
  Printf.eprintf fmt

let errorl loc =
  output_string stderr (range loc ^ "\n")

let pp_range ppf loc = Fmt.string ppf (range loc)

let error_fmt =
  Fmt.(vbox (pp_range ++ cut))
