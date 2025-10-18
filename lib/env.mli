open Ast
open Asttypes

type t

val lookup : ident -> t -> symbol_info option

val extend : ident -> symbol_info -> t -> t

val empty_env : t

val mem : ident -> t -> bool

val fold : (ident -> symbol_info -> 'a -> 'a) -> t -> 'a -> 'a

val is_used : ident -> t -> bool

val mark_used : ident -> t -> unit

val is_bound : ident -> t -> bool

val mark_bound : ident -> t -> unit

val is_updated : ident -> t -> bool

val mark_updated : ident -> t -> unit