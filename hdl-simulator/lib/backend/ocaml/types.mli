type typ
type typ_decl

(** {2 Helper Functions} *)

(** {3 OCaml types} *)

(** OCaml [unit]*)
val tunit : typ

(** OCaml [int] *)
val tint : typ

(** OCaml [string] *)
val tstring : typ

(** OCaml [bool]*)
val tbool : typ

(** User-defined type *)
val tid : string -> typ

(** OCaml [t1 -> t2] *)
val tarrow : typ -> typ -> typ

(** OCaml tuple [(t1,...,tn)] *)
val ttuple : typ list -> typ

(** OCaml object [<f1 : t1 ; ... ; fn : tn >] *)
val tobject : typ Map.Make(String).t -> typ

(** OCaml array [t array] *)
val tarray : typ -> typ

(** OCaml type inside a module [M.t] *)
val tmodpath : string -> string -> typ

(** Ocaml polymorphic type instantiations [tvar t] *)
val tpoly : typ list -> typ -> typ

(** OCaml module signature type [module SIG] *)
val tmodsig : string -> typ

(** {3 Type Declarations} *)

(** Type alias *)
val decl_alias : string -> typ -> typ_decl

(** Record declaration *)
val decl_record : string -> typ Map.Make(String).t -> typ_decl

(** Variant declaration *)
val decl_variant : string -> typ list Map.Make(String).t -> typ_decl


module Pretty : sig
  val pp_typ : typ Fmt.t
  val pp_typ_decl : typ_decl Fmt.t
end
