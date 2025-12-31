(*
    parse.mli
*)


(* Parse a line. *)
val parse_line_opt : string -> Ast.t option
val parse_line : string -> Ast.t

