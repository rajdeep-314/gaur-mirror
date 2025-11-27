open Asttypes

(** The typing environment, with keys as strings and values as HDL types. *)
type t

(** [lookup id env] returns [Some t] if [id] is bound to type [t] in environment
    [env], and [None] otherwise.
*)
val lookup : ident -> t -> typ option

(** [extend id typ env] is [env] with [id] bound to [typ].*)
val extend : ident -> typ -> t -> t

(** [empty_env] is the empty environment. *)
val empty_env : t

(** [mem id env] returns [true] if there is a binding for [id] in [env],
    and [false] otherwise.
*)
val mem : ident -> t -> bool

(** [fold f env init] computes [(f idN tN ... (f id1 t1 init)...)],
    where [id1 ... idN] are the keys of all bindings in [env]
    (in increasing order), and [t1 ... tN] are the respective types.
*)
val fold : (ident -> typ -> 'a -> 'a) -> t -> 'a -> 'a

(** [print env] prints the environment contents. *)
val print : t -> unit
