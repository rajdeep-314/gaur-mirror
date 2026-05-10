module E = Expressions
module P = Patterns
module T = Types

let splice bv lb ub = E.eapply (E.eid "splice") [bv; lb; ub]

let concat e1 e2 = E.eapply (E.eid "concat") [e1; e2]

let fst e = E.eapply (E.eid "fst") [e]

let snd e = E.eapply (E.eid "snd") [e]

let pp_bv = E.eid "pp_bv"

let fmt f = E.eapply (E.modpath "Fmt" f)

let fmt_pf = fmt "pf"

let fmt_pf_ppf el = fmt_pf (E.eid "ppf" :: el)

let fmt_t v = T.tpoly [v] (T.tmodpath "Fmt" "t")

let tbits n = E.eapply (E.eid "tbits") [E.eint n]

let trecord_of_list el = E.eapply (E.eid "trecord_of_list") [E.elist el]

let ebits ebv = E.eapply (E.eid "ebits") [ebv]

let erecord_of_list el = E.eapply (E.eid "erecord_of_list") [E.elist el]

let pipe = E.einfix "|>"
let ( ||> ) = pipe

let get_bv = E.eid "get_bv"

let get_record = E.einfix "|>?"
let ( ||>? ) = get_record

let raise_fun exn = E.eapply (E.eid "raise") [ exn ]
let raise_invalid_argument_fun e =
  raise_fun (E.evar "Invalud_argument" [e])
let repl = E.eid "repl"
let stdin = E.eid "stdin"
let stdout = E.eid "stdout"
let inx_open_txt e = E.eapply (E.modpath "In_channel" "open_txt") [e]
let outx_open_txt e = E.eapply (E.modpath "Out_channel" "open_txt") [e]
let array_length e = E.eapply (E.modpath "Array" "length") [e]
let sys_argv = E.modpath "Sys" "argv"
let exit n = E.eapply (E.eid "exit") [E.eint n]
