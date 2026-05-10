open Expressions
open Types

type item =
  | Typ_decl of typ_decl
  | Let_bind of let_bind

type decl =
  { name : string
  ; items : item list
  }


let mdecl id items =
  { name=id
  ; items=items
  }

let item_typ_decl t = Typ_decl t
let item_let_bind lb = Let_bind lb


module Pretty = struct
  let cutcut ppf = Fmt.(cut ++ cut) ppf

  let pp_item ppf item =
    match item with
    | Typ_decl t -> Types.Pretty.pp_typ_decl ppf t
    | Let_bind lb -> Expressions.Pretty.pp_blet ppf lb

  let pp_decl ppf m =
    Fmt.pf ppf "@[<v 0>module %s = struct@,  @[<v 0>%a@]@,end@]"
      m.name
      (Fmt.list ~sep:cutcut pp_item) m.items

  let pp_decl_list ppf ml =
    Fmt.list pp_decl ppf ml
end
