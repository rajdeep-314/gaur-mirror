module E = Expressions

type item =
  | Open    of string
  | LetFun  of E.let_bind

type file = item list

let fopen s = Open s
let ffun lb = LetFun lb


module Pretty = struct
  let pp_item ppf i =
    match i with
    | Open    s  -> Fmt.pf ppf "open %s"
                      (String.capitalize_ascii s)
    | LetFun  lb -> E.Pretty.pp_blet ppf lb
end
