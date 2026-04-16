open Parse

type 'a t = (string, 'a Ast.chip_info) Hashtbl.t

let add (tbl : 'a t) (ci : 'a Ast.chip_info) : unit =
  Hashtbl.add tbl ci.data.name ci

let make (p : 'a Ast.prog) : 'a t =
  let tbl = Hashtbl.create 16 in
  List.iter (add tbl) p;
  tbl

let find (tbl : 'a t) name = Hashtbl.find tbl name
