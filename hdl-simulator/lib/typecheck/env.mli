open Parse
open Parse.Types

(** The typing environment, with keys as strings and values as HDL
    types.
*)
type t

(** [find_opt id env] is [Some t] if [id] is bound to type [t] in
    environment [env], and [None] otherwise.
*)
val find_opt : ident -> t -> Types.t option

(** [add id typ env] is [env] with [id] bound to [typ].*)
val add : ident -> Types.t -> t -> t

(** [empty] is the empty environment. *)
val empty : t

(** [mem id env] returns [true] if there is a binding for [id] in
    [env], and [false] otherwise.
*)
val mem : ident -> t -> bool

(** [fold f env init] computes [(f idN tN ... (f id1 t1 init)...)],
    where [id1 ... idN] are the keys of all bindings in [env]
    (in increasing order), and [t1 ... tN] are the respective types.
*)
val fold : (ident -> Types.t -> 'a -> 'a) -> t -> 'a -> 'a

module Pretty : sig
  (** Pretty print the environment *)
  val pp : t Fmt.t
end
