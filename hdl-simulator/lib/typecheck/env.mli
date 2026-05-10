open Parse
open Parse

(** The typing environment, with keys as strings and values as HDL
    types.
*)
type t

(** [find_opt id env] is [Some t] if [id] is bound to type [t] in
    environment [env], and [None] otherwise.
*)
val find_opt : string -> t -> Types.t option

(** [add id typ env] is [env] with [id] bound to [typ].*)
val add : string -> Types.t -> t -> t

(** [empty] is the empty environment. *)
val empty : t

(** [singleton x y] returns the one-element map that contains a
    binding [y] for [x].
*)
val singleton : string -> Types.t -> t

(** [mem id env] returns [true] if there is a binding for [id] in
    [env], and [false] otherwise.
*)
val mem : string -> t -> bool

(** [fold f env init] computes [(f idN tN ... (f id1 t1 init)...)],
    where [id1 ... idN] are the keys of all bindings in [env]
    (in increasing order), and [t1 ... tN] are the respective types.
*)
val fold : (string -> Types.t -> 'a -> 'a) -> t -> 'a -> 'a

module Pretty : sig
  (** Pretty print the environment *)
  val pp : t Fmt.t
end
