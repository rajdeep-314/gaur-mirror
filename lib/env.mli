open Ast

type t

val lookup : ident -> t -> typ option

val extend : ident -> typ -> t -> t

val empty_env : t

val mem : ident -> t -> bool

val fold : (ident -> typ -> 'a -> 'a) -> t -> 'a -> 'a
