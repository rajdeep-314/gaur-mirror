(*
    parser.mli
*)


(** {1 Parser Module} *)

(** [parse_maps str] parses a string [str] of maps to obtain the AST maps. *)
val parse_maps : string -> Ast.maps

