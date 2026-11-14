(** Runtime values of the interpreter.

    All runtime trees share one polymorphic shape, parameterised by the value
    at the leaves (one leaf per bit):
    - [bool]                    : plain snapshot values
    - [bool Incr.Var.t]         : the unwatched input/state variables
    - [bool Incr.t]             : nodes of the incremental computation DAG
    - [bool Incr.Observer.t]    : observers of the DAG outputs
*)

open Ast

module Incr = Incremental.Make ()
module Var  = Incr.Var

exception Shape_mismatch of string

let mismatch fmt = Printf.ksprintf (fun s -> raise (Shape_mismatch s)) fmt

(** {1 Shapes} *)

type 'a shape =
  | Bitvec of 'a array
  | Record of 'a shape StrMap.t

(** State of a chip instance is
    - its own state variables
    - recursively, the states of its child instances keyed by their
    instance ids.
*)
type 'a state =
  { self : 'a shape StrMap.t
  ; recu : 'a state IntMap.t
  }

(** Input interface of a chip instance is
    - values applied to its input port
    - the current state of the chip and all its child instances.
*)
type 'a input_interface =
  { input : 'a shape
  ; state : 'a state
  }

(** Output interface of a chip instance is
    - values produced on its output port
    - the next state of the chip and all its child instances to be latched on clock.
*)
type 'a output_interface =
  { output : 'a shape
  ; state  : 'a state
  }

(** Type of a chip state, reflecting {!state}. *)
type state_tipe =
  { self : tipe StrMap.t
  ; recu : state_tipe IntMap.t
  }

type bit   = bool Incr.t
type value = bit shape

(** {1 Construction} *)

let rec create (t : tipe) (f : unit -> 'a) : 'a shape =
  match t with
  | TBitvec n -> Bitvec (Array.init n (fun _ -> f ()))
  | TRecord r -> Record (StrMap.map (fun t -> create t f) r)

let rec create_state (t : state_tipe) (f : unit -> 'a) : 'a state =
  { self=StrMap.map (fun t -> create t f) t.self
  ; recu=IntMap.map (fun t -> create_state t f) t.recu
  }

let create_input_interface (input_t : tipe) (state_t : state_tipe) f : 'a input_interface =
  { input=create input_t f
  ; state=create_state state_t f
  }

(** {1 Mapping} *)

let rec map (f : 'a -> 'b) (v : 'a shape) : 'b shape =
  match v with
  | Bitvec bv -> Bitvec (Array.map f bv)
  | Record r  -> Record (StrMap.map (map f) r)

let rec map_state (f : 'a -> 'b) (s : 'a state) : 'b state =
  { self=StrMap.map (map f) s.self
  ; recu=IntMap.map (map_state f) s.recu
  }

let map_input_interface f (i : 'a input_interface) : 'b input_interface =
  { input=map f i.input
  ; state=map_state f i.state
  }

let map_output_interface f (o : 'a output_interface) : 'b output_interface =
  { output=map f o.output
  ; state=map_state f o.state
  }

(** {1 Traversals} *)

(** [iter2_bindings name f l1 l2] applies [f] on the values bound to equal
    keys of two sorted binding lists, which must have exactly the same keys. *)
let iter2_bindings name f l1 l2 =
  if List.length l1 <> List.length l2 then mismatch "%s: different number of fields" name;
  List.iter2
    (fun (k1, v1) (k2, v2) ->
       if k1 <> k2 then mismatch "%s: different fields" name;
       f v1 v2)
    l1 l2

let rec iter2 (f : 'a -> 'b -> unit) (v1 : 'a shape) (v2 : 'b shape) : unit =
  match v1, v2 with
  | Bitvec b1, Bitvec b2 ->
    if Array.length b1 <> Array.length b2
    then mismatch "bit vectors of lengths %d and %d" (Array.length b1) (Array.length b2);
    Array.iter2 f b1 b2
  | Record r1, Record r2 ->
    iter2_bindings "record" (iter2 f) (StrMap.bindings r1) (StrMap.bindings r2)
  | Bitvec _, Record _
  | Record _, Bitvec _ -> mismatch "bit vector and record"

let rec iter (f : 'a -> unit) (v : 'a shape) : unit =
  match v with
  | Bitvec b -> Array.iter f b
  | Record r -> StrMap.iter (fun _ v -> iter f v) r

let rec iter_state (f : 'a -> unit) (s : 'a state) : unit =
  StrMap.iter (fun _ v -> iter f v) s.self;
  IntMap.iter (fun _ sub -> iter_state f sub) s.recu

let rec iter2_state (f : 'a -> 'b -> unit) (s1 : 'a state) (s2 : 'b state) : unit =
  iter2_bindings "state" (iter2 f) (StrMap.bindings s1.self) (StrMap.bindings s2.self);
  iter2_bindings "child states" (iter2_state f) (IntMap.bindings s1.recu) (IntMap.bindings s2.recu)


(** {1 Accessors} *)

let get_field (v : 'a shape) (f : string) : 'a shape =
  match v with
  | Record r ->
    (match StrMap.find_opt f r with
     | Some v -> v
     | None -> mismatch "no field %s" f)
  | Bitvec _ -> mismatch "field %s accessed on a bit vector" f

let get_bitvec (v : 'a shape) : 'a array =
  match v with
  | Bitvec bv -> bv
  | Record _ -> mismatch "expected a bit vector, got a record"

(** [splice lb ub bv] is the bits of [bv] from [lb] to [ub], both inclusive. *)
let splice lb ub bv = Array.sub bv lb (ub - lb + 1)
