(** {1 Initial environment for typechecking} *)

(** [v prog] returns an environment which contains the standard [Nand]
    chip and all chips in [prog].
*)
val v : 'a. 'a Parse.Ast.prog -> Env.t
