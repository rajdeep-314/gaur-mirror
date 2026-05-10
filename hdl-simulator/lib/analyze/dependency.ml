open Parse

module Chips : sig
  val rhs      : 'a Ast.rhs_info    -> string option
  val assign   : 'a Ast.assign_info -> string option
  val chip     : 'a Ast.chip_info   -> string list
  val chip_all : 'a Ast.prog -> (string * string list) list
end = struct
  let rhs (ri : 'a Ast.rhs_info) : string option =
    match ri.data with
    | Call (id, _) -> Some id
    | Expr _       -> None

  let assign (ai : 'a Ast.assign_info) : string option =
    match ai.data with
    | Current (_, ri)
    | Next    (_, ri) -> rhs ri

  let rec filter_some l =
    match l with
    | []      -> []
    | x :: xs ->
      match x with
      | None   -> filter_some xs
      | Some y -> y :: filter_some xs

  let chip (ci : 'a Ast.chip_info) : string list =
    List.map assign ci.data.parts
    |> filter_some

  let chip_all (p : 'a Ast.prog) : (string * string list) list =
    p
    |> List.map (fun ci -> Ast.Mnemonics.chip_info_name ci, chip ci)
end
