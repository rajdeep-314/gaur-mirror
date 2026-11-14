(** Interpreter: compiles every chip into a function from its input interface
    to its output interface, over nodes of the incremental computation DAG.

    Calling the function of a chip once builds the DAG of one instance of that
    chip (and, recursively, of all its child instances). *)

open Ast
open Value

exception Interp_error of string

let error fmt = Printf.ksprintf (fun s -> raise (Interp_error s)) fmt

(** The compiled function of a chip. *)
type chip_fn = bit input_interface -> bit output_interface

(** A compiled chip together with the type of its input interface. *)
type chip_def =
  { input_t : tipe
  ; state_t : state_tipe
  ; fn      : chip_fn
  }

type chip_env = chip_def StrMap.t

let empty_state_t : state_tipe = { self=StrMap.empty; recu=IntMap.empty }
let empty_state   : 'a state   = { self=StrMap.empty; recu=IntMap.empty }

module Builtin = struct
  module Nand = struct
    let name = "Nand"

    let def : chip_def =
      { input_t=TRecord (StrMap.of_list [("a", TBitvec 1); ("b", TBitvec 1)])
      ; state_t=empty_state_t
      ; fn=(fun inputs ->
          let bit f = (get_bitvec (get_field inputs.input f)).(0) in
          { output=Bitvec [| Incr.map2 ~f:(fun x y -> not (x && y)) (bit "a") (bit "b") |]
          ; state=empty_state
          })
      }
  end

  let env : chip_env = StrMap.of_list [ (Nand.name, Nand.def) ]
end

let find_chip (env : chip_env) name =
  match StrMap.find_opt name env with
  | Some def -> def
  | None -> error "unknown chip %s" name

(** {1 Chip bodies} *)

(** Values visible while evaluating the body of one chip instance. *)
type scope =
  { input  : value                 (** chip input port values *)
  ; state  : bit state             (** current state of this chip & child instances *)
  ; locals : value StrMap.t        (** intermediate wires defined so far *)
  }

let lookup (chip : chip) (sc : scope) (kind : var) (x : string) : value =
  let find what m =
    match StrMap.find_opt x m with
    | Some v -> v
    | None -> error "chip %s: unbound %s %s" chip.name what x
  in
  match kind with
  | Input -> sc.input
  | State -> find "state" sc.state.self
  | Inter -> find "variable" sc.locals

let rec interp_expr chip (sc : scope) (e : expr) : value =
  match e with
  | Const bv ->
    Bitvec (bv |> Hdlutils.NonEmpty.to_list |> List.map Incr.const |> Array.of_list)
  | Record r -> Record (StrMap.map (interp_expr chip sc) r)
  | Splice (e, lb, ub) ->
    Bitvec (interp_expr chip sc e |> get_bitvec |> splice lb ub)
  | Concat (e1, e2) ->
    let b1 = interp_expr chip sc e1 |> get_bitvec in
    let b2 = interp_expr chip sc e2 |> get_bitvec in
    Bitvec (Array.append b1 b2)
  | Access (kind, x) -> lookup chip sc kind x
  | Member (e, f) -> get_field (interp_expr chip sc e) f

(** Evaluates a right-hand side. For a call, it also returns the next state of
    the called instance, together with its instance id. *)
let interp_rhs env chip (sc : scope) (r : rhs) : value * (int * bit state) option =
  match r with
  | Expr e -> interp_expr chip sc e, None
  | Call (name, id, e) ->
    let def = find_chip env name in
    let state =
      match IntMap.find_opt id sc.state.recu with
      | Some s -> s
      | None -> error "chip %s: missing state of instance %d of %s" chip.name id name
    in
    let out = def.fn { input=interp_expr chip sc e; state } in
    out.output, Some (id, out.state)

let interp_chip (env : chip_env) (chip : chip) : chip_fn =
  fun inputs ->
    let step (sc, (next : bit state)) (a : assign) =
      let rhs = match a with Current (_, r) | Next (_, r) -> r in
      let v, child = interp_rhs env chip sc rhs in
      let next =
        match child with
        | None -> next
        | Some (id, s) -> { next with recu=IntMap.add id s next.recu }
      in
      match a with
      | Current (x, _) -> { sc with locals=StrMap.add x v sc.locals }, next
      | Next (x, _)    -> sc, { next with self=StrMap.add x v next.self }
    in
    (* A state variable which is never assigned keeps its current value. *)
    let init_next : bit state = { self=inputs.state.self; recu=IntMap.empty } in
    let init_sc = { input=inputs.input; state=inputs.state; locals=StrMap.empty } in
    let sc, next =
      List.fold_left step (init_sc, init_next) chip.parts
    in
    { output=lookup chip sc Inter chip.output.name
    ; state=next
    }

(** {1 Programs} *)

(** Instance ids and names of the chips called in the body of [chip]. *)
let calls (chip : chip) : (int * string) list =
  List.filter_map
    (fun a ->
       match a with
       | Current (_, Call (name, id, _))
       | Next (_, Call (name, id, _)) -> Some (id, name)
       | Current (_, Expr _) | Next (_, Expr _) -> None)
    chip.parts

let state_tipe (env : chip_env) (chip : chip) : state_tipe =
  { self=chip.states
  ; recu=
      List.fold_left
        (fun acc (id, name) -> IntMap.add id (find_chip env name).state_t acc)
        IntMap.empty (calls chip)
  }

let add_chip (env : chip_env) (chip : chip) : chip_env =
  (* Builtin chips (e.g. the Nand declaration inserted by the typechecker)
     have no body to interpret; keep the builtin implementation. *)
  if StrMap.mem chip.name Builtin.env then env
  else
    StrMap.add chip.name
      { input_t=chip.input.tipe
      ; state_t=state_tipe env chip
      ; fn=interp_chip env chip
      }
      env

(** [nand_counts p] computes the static number of [Nand] gates instantiated
    by each chip in [p] (which must be in topological order). *)
let nand_counts (p : prog) : int StrMap.t =
  let init = StrMap.singleton Builtin.Nand.name 1 in
  List.fold_left
    (fun acc (chip : chip) ->
       if StrMap.mem chip.name init then acc
       else
         let count =
           List.fold_left
             (fun sum (_, callee) ->
                sum + (StrMap.find_opt callee acc |> Option.value ~default:0))
             0
             (calls chip)
         in
         StrMap.add chip.name count acc)
    init
    p

(** [interp_prog p] compiles every chip of [p]. Chips must be in topological
    order: callees before callers. *)
let interp_prog (p : prog) : chip_env =
  List.fold_left add_chip Builtin.env p
