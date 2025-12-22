(*
    parser.mli
*)


module Ram = Cpusim.Ram

(* read_half bs n reads the n'th half-word in the byte-string bs. *)
val read_half : Bytes.t -> int -> int

(* parse_bytes bs maps the byte-string bs to a RAM value. *)
val parse_bytes : Bytes.t -> Ram.t

(* parse_file p parses bytes in the file with path p. *)
val parse_file : string -> Ram.t

