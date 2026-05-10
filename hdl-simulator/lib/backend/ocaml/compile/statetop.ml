module M = Modules

open Parse

let chip is_stfl (ci : 'a Ast.chip_info) =
  if is_stfl ci.data.name
  then Stateful.chip is_stfl ci
  else Stateless.chip ci

let prog is_stfl (p : 'a Ast.prog) =
  List.map (chip is_stfl) p


module Make (P : sig type t val prog : t Ast.prog end) () : sig
  val v : M.decl list
end = struct
  module S = Analyze.Statefulness.Make(P)()

  let v = List.tl (prog S.is_stateful P.prog)
end
