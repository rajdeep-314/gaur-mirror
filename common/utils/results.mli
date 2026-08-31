(*
    results.mli
*)


(** [flatten_results rs] returns [Ok (x1, x2, ..., xn)] if [rs] is [[Ok x1; Ok
    x2; ...; Ok xn]], and [Error e] if the first error entry in [rs] is [Error
    e]. *)
val flatten_results : ('a, 'e) result list -> ('a list, 'e) result

