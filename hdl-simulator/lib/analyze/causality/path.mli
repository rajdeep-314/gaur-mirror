(** Structured paths representing wire, port, and state endpoints in hardware circuits. *)

type t =
  { root    : string
  ; fields  : string list
  ; is_next : bool
  }

(** [make ?fields ?is_next root] creates a new path. *)
val make : ?fields:string list -> ?is_next:bool -> string -> t

(** [var name] creates a simple identifier path without fields or prime. *)
val var : string -> t

(** [port name fields] creates a port path with the given subfields. *)
val port : string -> string list -> t

(** [state name] creates a current-cycle state path. *)
val state : string -> t

(** [next_state name] creates a next-cycle state target path (rendered as [name']). *)
val next_state : string -> t

(** [add_field path field] appends [field] to [path.fields]. *)
val add_field : t -> string -> t

(** [add_fields path fields] appends multiple [fields] to [path.fields]. *)
val add_fields : t -> string list -> t

(** [match_port expected_port path] returns [Some fields] if [path.root = expected_port],
    otherwise [None]. Eliminates manual string prefix stripping. *)
val match_port : string -> t -> string list option

val to_string : t -> string
val pp : t Fmt.t

val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int
