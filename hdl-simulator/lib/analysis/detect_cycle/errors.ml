type cycle = string list

type err =
  | Cycles of cycle list

module Printers : sig
  val err : err -> string
end = struct
  let report_cycle c =
    Printf.sprintf "%s"
      (List.fold_left
         (fun v vs' -> Printf.sprintf "%s -- %s" v vs')
         (List.hd c)
         (List.tl c)
      )

  let err e =
    match e with
    | Cycles cs ->
      [ Printf.sprintf "Error: Cyclic Dependency."; ]
      @ List.flatten [List.map report_cycle cs]
      |> String.concat "\n"
end
