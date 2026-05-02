(*
    repl_test.ml

    Testing the REPL.
*)


module UF = Utils.Files
module VE = Vm.Emulator
module E  = Vm.Syntax.Parsing.Err
module Er = VE.Err
module I  = VE.State.Inspect
module P  = VE.State.PP

module R = VE.Repl

module STbl = Hashtbl.Make (String)
type src_tbl = string STbl.t


let argl = Array.to_list Sys.argv
let ps =
    match argl with
    | [] -> []
    | _ :: xs -> xs

let ps, ps_scs =
    try
        List.split
        (List.map Vm.Syntax.Parsing.Vm.parse_from_path ps)
    with
    | Vm.Syntax.Parsing.Err.LexingErrorWI _ as e ->
        prerr_endline (Printexc.to_string e) ;
        exit 1
    | Vm.Syntax.Parsing.Err.ParsingErrorWI _ as e ->
        prerr_endline (Printexc.to_string e) ;
        exit 1


let src_tbl : src_tbl =
    STbl.of_seq (List.to_seq ps_scs)


let () =
    try
        let ip = VE.Vm2ir.translate_progs "main" ps in
        R.main_loop src_tbl ip
    with
    | Er.SemanticError (a, e) ->
        prerr_endline (Er.pp_semantic_error src_tbl (a, e)) ;
        exit 3

