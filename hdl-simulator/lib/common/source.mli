(** Source representations for files and in-memory buffers. *)

(** Physical or in-memory origin from which source content is retrieved. *)
type origin =
  | Disk of { path : string } (** File stored on the local filesystem at [path]. *)
  | Memory of { content : string }
  (** In-memory string content (e.g. from an editor buffer, REPL, or test fixture). *)

(** A named source unit, pairing a logical display name with its origin. *)
type t =
  { name : string
    (** Identifier or display filename (e.g. ["chip.ghdl"] or ["<editor>"]). *)
  ; origin : origin (** Origin from which source content is retrieved. *)
  }

(** [from_file path] creates a source referencing a file on disk. *)
val from_file : string -> t

(** [from_string ~name content] creates a source from an in-memory string. *)
val from_string : name:string -> string -> t

val name : t -> string
val origin : t -> origin

(** [get_lines source ~first ~last] extracts 1-indexed lines [first]
    through [last] inclusive.
    Returns [(line_number, line_text)] pairs.
*)
val get_lines : t -> first:int -> last:int -> (int * string) list

(** [resolver] maps a filename or identifier to a [Source.t]. *)
type resolver = string -> t option

(** [default_resolver] checks disk filesystem, returning [Some (from_file path)] if it exists. *)
val default_resolver : resolver

(** [of_list sources] resolves names against an in-memory list of sources. *)
val of_list : t list -> resolver

(** [of_string_lookup f] adapts a simple [(string -> string option)] function into a resolver. *)
val of_string_lookup : (string -> string option) -> resolver
