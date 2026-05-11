module E = Expressions
module F = Functions
module N = Name
module P = Patterns
module T = Types

let select_fail_match =
  E.ematch
    (E.eapply (E.eid N.fun_select) [E.eid "name"])
    [ P.pvariant "Some" [P.pmodule "C"], E.emodule "C"
    ; P.pvariant "None" [], F.raise_invalid_argument_fun (E.eid "name")
    ]

let select_fail_fun =
  E.bfun
    "select_fail"
    [P.pid "name", None]
    (Some (T.tmodsig "CHIP"))
    select_fail_match


let run_repl_expr =
  E.eapply
    F.repl
    [ F.stdin
    ; F.stdout
    ; (E.eapply (E.eid "select_fail") [E.eid "name"])
    ; E.estring "REPL"
    ; E.etrue
    ; E.efalse
    ]

let run_repl_fun =
  E.bfun
    "run_repl"
    [P.pid "name", None]
    None
    run_repl_expr


let run_bench_expr =
  let inx_p = P.pid "inx" in
  let outx_p = P.pid "outx" in
  let inx_e = E.eid "inx" in
  let outx_e = E.eid "outx" in
  let inf_e = E.eid "inf" in
  let outf_e = E.eid "outf" in
  E.elet inx_p (F.inx_open_txt inf_e) (
    E.elet outx_p (F.outx_open_txt outf_e) (
      E.eapply F.repl [
        inx_e
      ; outx_e
      ; E.eapply (E.eid "select_fail") [E.eid "name"]
      ; (E.earray_idx F.sys_argv 2)
      ; E.efalse
      ; E.etrue
      ]
    )
  )

let run_bench_fun =
  E.bfun
    "run_bench"
    [P.pid "name", None; P.pid "inf", None; P.pid "outf", None]
    None
    run_bench_expr


let run_match =
  let argc_e = E.eid "argc" in
  E.ematch argc_e [
    P.pint 0, F.exit 1
  ; P.pint 1, F.exit 1
  ; P.pint 2, E.eapply (E.eid "run_repl") [E.earray_idx F.sys_argv 1]
  ; P.pwild , E.eapply (E.eid "run_bench") [E.earray_idx F.sys_argv 1; E.earray_idx F.sys_argv 2; E.estring "test.out"]
  ]


let run_expr =
  let argc_p = P.pid "argc" in
  E.elet argc_p (F.array_length F.sys_argv) run_match

let run_fun =
  E.bfun
    "_"
    []
    None
    run_expr
