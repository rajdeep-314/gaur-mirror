open PHDL
open PHDL.ParseHelper

(** [parse s] parses [s] into an AST *)
let _parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.read lexbuf in
  ast

let bind_error r f =
  match r with
  | Ok a -> Ok a
  | Error e -> f e

let helper filename =
  bind_error (attempt1 filename) (attempt2 filename)

let accu p f =
  let fp = helper f in
  match fp with
  | Ok a -> a @ p
  | Error e -> e

let foo =
  let files = Array.to_list Sys.argv in
  List.fold_left accu [] (List.tl files)

module Dot = Graph.Graphviz.Dot(struct
    include Usedef_tree.UDGraph
    let edge_attributes _ = []
    let vertex_attributes _ = []
    let default_edge_attributes _ = []
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
    let get_subgraph _ = None
    let vertex_name v =
      Format.sprintf
        "\"%s\""
        (Usedef_tree.UDGraphV.string_of_vertex v)
  end)


let draw_graph (dgi: Usedef_tree.ud_graph_info) =
  let g = dgi.graph in
  let name = dgi.chip ^ ".dot" in
  let oc = open_out name in
  Dot.output_graph oc g;
  close_out oc


let () =
  let env = Init_env.init_chip foo in
  let tcp = Typecheck.tc_program env foo in
  (* let udtl = Usedef_check.mk_graph_program tcp in *)
  let udtl = Usedef_check.ud_check_program tcp in
  (
    List.iter draw_graph udtl;
  )


(* let () =
   for i = 1 to Array.length Sys.argv - 1 do
    let filename = Sys.argv.(i) in
    let result = attempt1 filename in
    match result with
    | Error text -> ignore (attempt2 filename text)
    | Ok program ->
      let env = Init_env.init_chip program in
      ignore (Typecheck.tc_program env program)
   done *)
