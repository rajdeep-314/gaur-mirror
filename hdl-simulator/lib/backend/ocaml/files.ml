module E = Expressions
module M = Modules

type item =
  | Open    of string
  | LetFun  of E.let_bind
  | Module  of M.decl

type file = item list

let fopen s = Open s
let ffun lb = LetFun lb
let fmodule m = Module m
let fmodule_list ml = List.map fmodule ml


module Pretty = struct
  let pp_item ppf i =
    match i with
    | Open    s  -> Fmt.pf ppf "open %s"
                      (String.capitalize_ascii s)
    | LetFun  lb -> E.Pretty.pp_blet ppf lb
    | Module  m  -> M.Pretty.pp_decl ppf m

  let pp_file ppf f = Fmt.list pp_item ppf f
end
