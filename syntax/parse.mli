(*
    parse.mli
*)


(* parse_prog p parses the textual PISA program p. *)
val parse_prog : string -> Ast.prog

(* parse_from_path p parses the content of the file with path p. *)
val parse_from_path : string -> Ast.prog

