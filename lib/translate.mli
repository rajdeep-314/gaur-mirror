(*
    translate.mli
*)


(* Adding a map to a binary format. *)
val add_map : Binary.t -> Ast.t -> int -> bool -> int -> unit

(* Translate a list of maps and obtain the resultant binary representation. *)
val translate_maps : Ast.maps -> int -> int -> bool -> int -> Ast.half -> Binary.t

