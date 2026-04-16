open Parse

(** Type of data structure holding chip names and their info. *)
type 'a t

(** Create a new DS from the given program. *)
val make : 'a Ast.prog -> 'a t

(** Find the info of a string in the DS. *)
val find : 'a t -> string -> 'a Ast.chip_info
