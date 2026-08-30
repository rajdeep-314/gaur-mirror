(*
    results.ml

    Utility functions related to the result type.
*)


let rec flatten_results = function
    | []          -> Ok []
    | Ok v :: rs  ->
      begin
      match flatten_results rs with
      | Error e -> Error e
      | Ok vs   -> Ok (v :: vs)
      end
    | Error e :: _  -> Error e

