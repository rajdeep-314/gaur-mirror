(** {1 Compiling PHDL identifiers to OCaml identifiers}  *)

(** {2 Prefixes} *)

(** Prefix of module name for a chip *)
let name_pfx = "chip_"

(** Prefix for variables in a chip *)
let var_pfx  = "var_"


(** {2 Chip IO} *)

(** [id_tin] is the OCaml type identifier of a chip.  *)
let id_tin = "tin"

(** [id_tout] is the OCaml type identifier of a chip.  *)
let id_tout = "tout"


(** {2 Chip Variables} *)

(** [var_current id] is the OCaml identifier for the
    current state variable [id].
*)
let var_current id =
  String.lowercase_ascii (var_pfx ^ id)

(** [var_next id] is the OCaml identifier for the
    next state variable [id].
*)
let var_next id =
  String.lowercase_ascii (var_pfx ^ id ^ "'")


(** {2 Chip Interface} *)

(** [module_name id] is the OCaml module identifier of a chip with
    name [id].
*)
let module_name id =
  String.capitalize_ascii (name_pfx ^ id)

(** [fun_out] is the name of a chip's, output function. *)
let fun_out = "out"

(** [fun_clk] is the name of a chip's clock function. *)
let fun_clk = "clk"
