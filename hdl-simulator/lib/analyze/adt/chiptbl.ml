open Parse

module Make (P : sig type a val prog : a Ast.prog end) () : sig
  type t

  val make : P.a Ast.prog -> unit

  val find : string -> P.a Ast.chip_info
end = struct
  type t = (string, P.a Ast.chip_info) Hashtbl.t

  let v : t = Hashtbl.create (List.length P.prog)

  let add (ci : P.a Ast.chip_info) : unit =
    Hashtbl.add v ci.data.name ci

  let make (p : P.a Ast.prog) =
    List.iter add p

  let find name = Hashtbl.find v name
end
