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

let tbits n = E.eapply (E.eid "tbits") [E.eint n]

let trecord_of_list el = E.eapply (E.eid "trecord_of_list") [E.elist el]
