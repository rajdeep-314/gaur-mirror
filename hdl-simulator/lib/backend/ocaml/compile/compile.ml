open Parse

(** [compile prog] compiles a program [prog] carrying no additional
    information.
*)
let compile p =
  let p = Rename.prog p in
  let module P = struct
    type t = unit

    let prog : t Ast.prog = p
  end
  in
  let module Q = Top.Make(P) () in
  List.tl Q.v
