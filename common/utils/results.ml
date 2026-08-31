(*
    results.ml

    Utility functions related to the result type.
*)


(** [flatten_results rs] returns [Ok (x1, x2, ..., xn)] if [rs] is [[Ok x1; Ok
    x2; ...; Ok xn]], and [Error e] if the first error entry in [rs] is [Error
    e]. *)
let rec flatten_results = function
    | []          -> Ok []
    | Ok v :: rs  ->
      begin
      match flatten_results rs with
      | Error e -> Error e
      | Ok vs   -> Ok (v :: vs)
      end
    | Error e :: _  -> Error e

