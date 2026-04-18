module E = Expressions
module P = Patterns
module T = Types

let splice bv lb ub = E.eapply (E.eid "splice") [bv; lb; ub]

let concat e1 e2 = E.eapply (E.eid "concat") [e1; e2]

let fst e = E.eapply (E.eid "fst") [e]

let snd e = E.eapply (E.eid "snd") [e]
