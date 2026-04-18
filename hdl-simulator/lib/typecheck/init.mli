(** {1 Initial environment for typechecking} *)

(** [env prog] returns an environment which contains the standard [Nand]
    chip and all chips in [prog].
*)
val env : 'a. 'a Parse.Ast.prog -> Env.t

(** [nane_name] is simply ["Nand"]. *)
val nand_name : string

(** [prog i p] is the program [p] with Nand chip information [i] added
    the head before [p].
*)
val prog : 'a -> 'a Parse.Ast.prog -> 'a Parse.Ast.prog
