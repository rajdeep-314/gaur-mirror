open Hdlutils

module StrSet = Set.Make (String)
module StrMap = Map.Make (String)

module PathPair = struct
  type t = Path.t * Path.t

  let compare (i1, o1) (i2, o2) =
    let c = Path.compare i1 i2 in
    if c <> 0 then c else Path.compare o1 o2
  ;;
end

module PathSet = Set.Make (PathPair)

type t =
  { name : string
  ; comb_paths : PathSet.t
  }

let create name = { name; comb_paths = PathSet.empty }

let add_path ~inp ~outp summary =
  { summary with comb_paths = PathSet.add (inp, outp) summary.comb_paths }
;;

let has_path ~inp ~outp summary = PathSet.mem (inp, outp) summary.comb_paths
let paths summary = PathSet.elements summary.comb_paths

module Env = struct
  type env = t StrMap.t

  let empty : env = StrMap.empty
  let add (summary : t) (e : env) : env = StrMap.add summary.name summary e
  let find_opt (name : string) (e : env) : t option = StrMap.find_opt name e

  (** Initial environment with built-in primitive Nand *)
  let init : env =
    let nand_name = Typecheck.Init.nand_name in
    let nand_summary =
      create nand_name
      |> add_path ~inp:(Path.port "in" [ "a" ]) ~outp:(Path.port "out" [])
      |> add_path ~inp:(Path.port "in" [ "b" ]) ~outp:(Path.port "out" [])
    in
    add nand_summary empty
  ;;
end
